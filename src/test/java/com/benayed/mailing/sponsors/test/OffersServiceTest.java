package com.benayed.mailing.sponsors.test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import java.time.Duration;
import java.time.LocalDateTime;
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
import com.benayed.mailing.sponsors.dto.SuppressionInfoDto;
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
	public void should_throw_exception_when_offer_has_no_sponsor() {
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
		Optional<SuppressionInfoDto> emptySuppression = Optional.empty();
		
		when(offerDBRepository.findById(someOfferId)).thenReturn(someOfferData);
		when(hiPathPlatformRepository.fetchOfferSuppressionInfo(null, null,null)).thenReturn(emptySuppression);
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
		Optional<SuppressionInfoDto> existingSuppression = Optional.of(SuppressionInfoDto.builder().build());
		
		when(offerDBRepository.findById(someOfferId)).thenReturn(someOfferData);
		when(hiPathPlatformRepository.fetchOfferSuppressionInfo(offerInTheDb.getCampaignid(), offerInTheDb.getSponsor().getApiKey(), offerInTheDb.getSponsor().getApiURL())).thenReturn(existingSuppression);
		
		//Act
		SuppressionInfoDto suppressionData = offerService.fetchOfferSuppressionData(someOfferId);
		
		//Assert
		Assertions.assertThat(suppressionData).isNotNull();
	}
	
	
	
	
	
	
	@Test
	public void should_throw_exception_when_refreshing_sponsor_data_while_no_sponsor_found() {
		//Arrange
		Long someSponsorId = 7l;
		Optional<SponsorEntity> emptySponsor = Optional.empty();
		when(sponsorDBRepository.findById(someSponsorId)).thenReturn(emptySponsor);
		
		//Act
		assertThrows(TechnicalException.class, () ->
		offerService.refreshSponsorOffers(someSponsorId));
		
		//Assert
		//Exception thrown
	}
	
	@Test
	public void should_throw_exception_when_refreshing_sponsor_data_with_null_platform() {
		//Arrange
		Long someSponsorId = 7l;
		Optional<SponsorEntity> sponsorWithNullPlatform = Optional.of(SponsorEntity.builder().build());
		when(sponsorDBRepository.findById(someSponsorId)).thenReturn(sponsorWithNullPlatform);
		
		//Act
		assertThrows(TechnicalException.class, () ->
		offerService.refreshSponsorOffers(someSponsorId));
		
		//Assert
		//Exception thrown
	}
	
	@Test
	public void should_fetch_offers_from_hiPath_Api_when_last_fetch_date_is_null() {
		//Arrange
		LocalDateTime nullLastRefreshDate = null;
		Long SomeSponsorId = 7l;
		SponsorEntity sponsorInTheRepo = SponsorEntity.builder()
				.platform(Platform.HiPath)
				.apiKey("someApiKey")
				.apiURL("someApiUrl")
				.id(SomeSponsorId)
				.name("someName")
				.lastOffersRefresh(nullLastRefreshDate)
				.build();
		List<OfferDto> offerReturnedByHiPath = Arrays.asList(OfferDto.builder().build());
		when(sponsorDBRepository.findById(SomeSponsorId)).thenReturn(Optional.of(sponsorInTheRepo));
		when(hiPathPlatformRepository.fetchOffers(sponsorInTheRepo.getApiKey(), sponsorInTheRepo.getApiURL())).thenReturn(offerReturnedByHiPath);
		
		//Act
		offerService.refreshSponsorOffers(SomeSponsorId);
		
		//Assert
		Mockito.verify(hiPathPlatformRepository, Mockito.times(1)).fetchOffers(sponsorInTheRepo.getApiKey(), sponsorInTheRepo.getApiURL());
		Mockito.verify(offerDBRepository, Mockito.times(1)).deleteBySponsor_id(sponsorInTheRepo.getId());
		Mockito.verify(sponsorDBRepository, Mockito.times(1)).save(sponsorInTheRepo);
		Assertions.assertThat(sponsorInTheRepo.getLastOffersRefresh()).isNotNull();
		Mockito.verify(offerDBRepository, Mockito.times(1)).saveAll(Mockito.anyIterable());
	}
	
	@Test
	public void should_fetch_offers_from_hiPath_Api_when_last_fetch_was_before_30_mins_ago() {
		//Arrange
		LocalDateTime fortyMinutesBeforeNow = LocalDateTime.now().minus(Duration.ofMinutes(40));
		Long SomeSponsorId = 7l;
		SponsorEntity sponsorInTheRepo = SponsorEntity.builder()
				.platform(Platform.HiPath)
				.apiKey("someApiKey")
				.apiURL("someApiUrl")
				.id(SomeSponsorId)
				.name("someName")
				.lastOffersRefresh(fortyMinutesBeforeNow)
				.build();
		List<OfferDto> offerReturnedByHiPath = Arrays.asList(OfferDto.builder().build());
		when(sponsorDBRepository.findById(SomeSponsorId)).thenReturn(Optional.of(sponsorInTheRepo));
		when(hiPathPlatformRepository.fetchOffers(sponsorInTheRepo.getApiKey(), sponsorInTheRepo.getApiURL())).thenReturn(offerReturnedByHiPath);
		
		//Act
		offerService.refreshSponsorOffers(SomeSponsorId);
		
		//Assert
		Mockito.verify(hiPathPlatformRepository, Mockito.times(1)).fetchOffers(sponsorInTheRepo.getApiKey(), sponsorInTheRepo.getApiURL());
		Mockito.verify(offerDBRepository, Mockito.times(1)).deleteBySponsor_id(sponsorInTheRepo.getId());
		Mockito.verify(sponsorDBRepository, Mockito.times(1)).save(sponsorInTheRepo);
		Assertions.assertThat(sponsorInTheRepo.getLastOffersRefresh()).isNotNull();
		Mockito.verify(offerDBRepository, Mockito.times(1)).saveAll(Mockito.anyIterable());
	}
	
	@Test
	public void should_not_fetch_offers_from_hiPath_Api_when_a_fetch_was_in_the_last_30_mins() { //cuz hipath only accepts a call to offers endpoint once each 30 mins 
		//Arrange
		LocalDateTime fifteenMinutesBeforeNow = LocalDateTime.now().minus(Duration.ofMinutes(15));
		Long SomeSponsorId = 7l;
		SponsorEntity sponsorInTheRepo = SponsorEntity.builder()
				.platform(Platform.HiPath)
				.apiKey("someApiKey")
				.apiURL("someApiUrl")
				.id(SomeSponsorId)
				.name("someName")
				.lastOffersRefresh(fifteenMinutesBeforeNow)
				.build();
		when(sponsorDBRepository.findById(SomeSponsorId)).thenReturn(Optional.of(sponsorInTheRepo));
		
		//Act
		offerService.refreshSponsorOffers(SomeSponsorId);
		
		//Assert
		Mockito.verify(hiPathPlatformRepository, Mockito.never()).fetchOffers(sponsorInTheRepo.getApiKey(), sponsorInTheRepo.getApiURL());
		Mockito.verify(offerDBRepository, Mockito.never()).deleteBySponsor_id(sponsorInTheRepo.getId());
		Mockito.verify(sponsorDBRepository, Mockito.never()).save(sponsorInTheRepo);
		Assertions.assertThat(sponsorInTheRepo.getLastOffersRefresh()).isEqualTo(fifteenMinutesBeforeNow);// did not change
		Mockito.verify(offerDBRepository, Mockito.never()).saveAll(Mockito.anyIterable());
	}
	
	
	
	
	
	
	
	
	@Test
	public void should_fetch_all_sponsor_data_and_map_offers_too(){
		//Arrange
		SponsorEntity sponsorWithAnOffer = SponsorEntity.builder()
				.offers(Arrays.asList(OfferEntity.builder().id(1l).build())).build();
		Mockito.when(sponsorDBRepository.findAll()).thenReturn(Arrays.asList(sponsorWithAnOffer));
		
		//Act
		List<SponsorDto> sponsorWithOffers = this.offerService.getAllSponsorsData();
		
		//Assert
		Mockito.verify(sponsorDBRepository, Mockito.times(1)).findAll();
		Assertions.assertThat(sponsorWithOffers.get(0).getOffers()).isNotEmpty(); //offers are mapped
		
	}
	
	
}
