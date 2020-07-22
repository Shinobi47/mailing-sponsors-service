package com.benayed.mailing.sponsors.test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.benayed.mailing.sponsors.dto.OfferDto;
import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.dto.SuppressionDataDto;
import com.benayed.mailing.sponsors.entity.OfferEntity;
import com.benayed.mailing.sponsors.entity.SponsorEntity;
import com.benayed.mailing.sponsors.enums.Platform;
import com.benayed.mailing.sponsors.exception.ResourceNotFoundException;
import com.benayed.mailing.sponsors.exception.TechnicalException;
import com.benayed.mailing.sponsors.repository.HiPathPlatformRepository;
import com.benayed.mailing.sponsors.repository.OfferDBRepository;
import com.benayed.mailing.sponsors.repository.SponsorDBRepository;
import com.benayed.mailing.sponsors.service.OfferService;
import com.benayed.mailing.sponsors.utils.DataMapper;


@ExtendWith(MockitoExtension.class)
class OffersServiceTest {

	@Mock
	private HiPathPlatformRepository hiPathPlatformRepository;
	
	@Mock
	private SponsorDBRepository sponsorDBRepository;
	
	@Mock
	private OfferDBRepository offerDBRepository;
	
	private DataMapper dataMapper = new DataMapper();
	
	private OfferService offerService;
	
	@BeforeEach
	public void init() {
		offerService = new OfferService(hiPathPlatformRepository, sponsorDBRepository, offerDBRepository, dataMapper);
	}
	
	@Test
	public void should_return_sponsorDto_when_it_exists_in_the_repository() {
		//Arrange
		String sponsorName = "someExistingSponsorName";
		Optional<SponsorEntity> entity = Optional.ofNullable(SponsorEntity.builder().build());
		when(sponsorDBRepository.findByName(sponsorName)).thenReturn(entity);
		
		//Act
		SponsorDto sponsor = offerService.fetchSponsorOffers(sponsorName);
		
		//Assert
		Assertions.assertThat(sponsor).isNotNull();
	}
	
	@Test
	public void should_throw_exception_when_sponsor_doesnt_exist() {
		//Arrange
		String sponsorName = "UnExistingSponsorName";
		Optional<SponsorEntity> entity = Optional.empty();
		when(sponsorDBRepository.findByName(sponsorName)).thenReturn(entity);
		
		//Act
		assertThrows(ResourceNotFoundException.class, () -> 
		offerService.fetchSponsorOffers(sponsorName));
		
		//Assert
		//=> Exception thrown
	}
	
	@Test
	public void should_throw_exception_when_no_offer_is_found() {
		//Arrange
		Long someOfferId = 1L;
		Optional<OfferEntity> emptyOffer = Optional.empty();
		when(offerDBRepository.findById(someOfferId)).thenReturn(emptyOffer);
		
		//Act
		assertThrows(TechnicalException.class, () -> 
		offerService.fetchOfferSuppressionData(someOfferId));
		
		//Assert
		// => exception thrown
	}
	
	@Test
	public void should_throw_exception_when_offer_has_no_sponsor_is_found() {
		//Arrange
		Long someOfferId = 1L;
		Optional<OfferEntity> entityWithNoSponsor = Optional.of(OfferEntity.builder().sponsor(null).build());
		when(offerDBRepository.findById(someOfferId)).thenReturn(entityWithNoSponsor);
		
		//Act
		assertThrows(TechnicalException.class, () -> 
		offerService.fetchOfferSuppressionData(someOfferId));
		
		//Assert
		// => exception thrown
	}
	
	@Test
	public void should_throw_exception_when_suppression_not_found() {
		//Arrange
		Long someOfferId = 1L;
		Optional<OfferEntity> someOfferData = Optional.of(OfferEntity.builder().sponsor(SponsorEntity.builder().build()).build());
		Optional<SuppressionDataDto> emptySuppression = Optional.empty();
		
		when(offerDBRepository.findById(someOfferId)).thenReturn(someOfferData);
		when(hiPathPlatformRepository.fetchOfferSuppressData(null, null,null)).thenReturn(emptySuppression);
		//Act
		assertThrows(ResourceNotFoundException.class, () -> 
		offerService.fetchOfferSuppressionData(someOfferId));
		
		//Assert
		// => exception thrown
	}

	@Test
	public void should_return_suppressionData_when_found() {
		//Arrange
		Long someOfferId = 1L;
		OfferEntity offerInTheDb = OfferEntity.builder()
		.campaignid("someCampaignId")
		.sponsor(SponsorEntity.builder().apiKey("someApiKey").apiURL("someApiUrl").build()).build();
		
		Optional<OfferEntity> someOfferData = Optional.of(offerInTheDb);
		Optional<SuppressionDataDto> existingSuppression = Optional.of(SuppressionDataDto.builder().build());
		
		when(offerDBRepository.findById(someOfferId)).thenReturn(someOfferData);
		when(hiPathPlatformRepository.fetchOfferSuppressData(offerInTheDb.getCampaignid(), offerInTheDb.getSponsor().getApiKey(), offerInTheDb.getSponsor().getApiURL())).thenReturn(existingSuppression);
		//Act
		SuppressionDataDto suppressionData = offerService.fetchOfferSuppressionData(someOfferId);
		
		//Assert
		Assertions.assertThat(suppressionData).isNotNull();
	}
	
	@Test
	public void should_throw_exception_when_refreshing_sponsor_data_with_no_sponsor_available() {
		//Arrange
		String someSponsorName = "someSponsorName";
		Optional<SponsorEntity> emptySponsor = Optional.empty();
		when(sponsorDBRepository.findByName(someSponsorName)).thenReturn(emptySponsor);
		
		//Act
		assertThrows(TechnicalException.class, () ->
		offerService.refreshSponsorOffers(someSponsorName));
		
		//Assert
		//Exception thrown
	}
	
	@Test
	public void should_throw_exception_when_refreshing_sponsor_data_with_null_platform() {
		//Arrange
		String someSponsorName = "someSponsorName";
		Optional<SponsorEntity> sponsorWithNullPlatform = Optional.of(SponsorEntity.builder().build());
		when(sponsorDBRepository.findByName(someSponsorName)).thenReturn(sponsorWithNullPlatform);
		
		//Act
		assertThrows(TechnicalException.class, () ->
		offerService.refreshSponsorOffers(someSponsorName));
		
		//Assert
		//Exception thrown
	}
	
	@Test
	public void should_throw_exception_when_refreshing_sponsor_data_with_null_platforma() {
		//Arrange
		String someSponsorName = "someSponsorName";
		SponsorEntity sponsorInTheRepo = SponsorEntity.builder()
				.platform(Platform.HiPath)
				.apiKey("someApiKey")
				.apiURL("someApiUrl")
				.id(1L)
				.name(someSponsorName)
				.build();
		List<OfferDto> offerReturnedByHiPath = Arrays.asList(OfferDto.builder().build());
		when(sponsorDBRepository.findByName(someSponsorName)).thenReturn(Optional.of(sponsorInTheRepo));
		when(hiPathPlatformRepository.fetchOffers(sponsorInTheRepo.getApiKey(), sponsorInTheRepo.getApiURL())).thenReturn(offerReturnedByHiPath);
		
		//Act
		offerService.refreshSponsorOffers(someSponsorName);
		
		//Assert
		Mockito.verify(hiPathPlatformRepository, Mockito.times(1)).fetchOffers(sponsorInTheRepo.getApiKey(), sponsorInTheRepo.getApiURL());
		Mockito.verify(offerDBRepository, Mockito.times(1)).deleteBySponsor_id(sponsorInTheRepo.getId());
		Mockito.verify(sponsorDBRepository, Mockito.times(1)).save(sponsorInTheRepo);
		Assertions.assertThat(sponsorInTheRepo.getLastOffersRefresh()).isNotNull();
		Mockito.verify(offerDBRepository, Mockito.times(1)).saveAll(Mockito.anyIterable());
	}

}
