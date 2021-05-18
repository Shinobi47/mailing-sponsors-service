package com.benayed.mailing.sponsors.test;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.entity.SponsorEntity;
import com.benayed.mailing.sponsors.enums.Platform;
import com.benayed.mailing.sponsors.exception.ResourceNotFoundException;
import com.benayed.mailing.sponsors.repository.OfferDBRepository;
import com.benayed.mailing.sponsors.repository.SponsorDBRepository;
import com.benayed.mailing.sponsors.service.OfferService;
import com.benayed.mailing.sponsors.service.SponsorService;
import com.benayed.mailing.sponsors.utils.DataMapper;

@ExtendWith(MockitoExtension.class)
public class SponsorServiceTest {
	
	private SponsorService sponsorService;
	
	@Mock
	private SponsorDBRepository sponsorDBRepository;
	
	@Mock
	private OfferDBRepository offerDBRepository;
	
	@Mock
	private DataMapper dataMapper;
	
	@Mock
	private OfferService offerService;
	
	@BeforeEach
	public void init() {
		sponsorService = new SponsorService(sponsorDBRepository, offerDBRepository, dataMapper, offerService);
	}
	


	// testing fetchSponsors()
	@Test
	public void should_call_repository_when_fetching_sponsors() {
		//Arrange
		when(sponsorDBRepository.findAll()).thenReturn(Arrays.asList(SponsorEntity.builder().build()));

		//Act
		sponsorService.fetchSponsors();
		
		//Assert
		verify(sponsorDBRepository, times(1)).findAll();
		verify(dataMapper, times(1)).toDto(any(SponsorEntity.class));
	}
	
	
	//testing fetchSponsorsWithOffers()
	@Test
	public void should_call_repository_when_fetching_sponsors_and_map_offers() {
		//Arrange
		when(sponsorDBRepository.findAll()).thenReturn(Arrays.asList(SponsorEntity.builder().build()));

		//Act
		sponsorService.fetchSponsorsWithOffers();
		
		//Assert
		verify(sponsorDBRepository, times(1)).findAll();
		verify(dataMapper, times(1)).toDto(any(SponsorEntity.class), eq(true));
	}
	

	
	//Testing deleteSponsor()
	@Test
	public void should_call_repositories_to_delete_sponsor_and_its_offers_when_deleting_sponsor() {
		//Arrange
		Long sponsorId = 1l;

		//Act
		sponsorService.deleteSponsor(sponsorId);
		
		//Assert
		verify(offerDBRepository, times(1)).deleteBySponsor_id(sponsorId);
		verify(sponsorDBRepository, times(1)).deleteById(sponsorId);
	}
	
	
	
	//Testing fetchSponsorWithOfferst()
	@Test
	public void should_call_repository_when_fetching_sponsor_and_map_offers() {
		//Arrange
		Long sponsorId = 1l;
		when(sponsorDBRepository.findById(sponsorId)).thenReturn(Optional.of(SponsorEntity.builder().id(1l).build()));
		when(dataMapper.toDto(any(SponsorEntity.class), eq(true))).thenReturn(SponsorDto.builder().build());
		
		//Act
		sponsorService.fetchSponsorWithOffers(sponsorId);
		
		//Assert
		verify(sponsorDBRepository, times(1)).findById(sponsorId);
		verify(dataMapper, times(1)).toDto(any(SponsorEntity.class), eq(true));
	}
	
	@Test
	public void should_throw_exception_when_no_sponsor_found_when_fetching_sponsor_by_id() {
		//Arrange
		Long sponsorId = 1l;
		
		//Act
		Assertions.assertThrows(ResourceNotFoundException.class, () -> 
		sponsorService.fetchSponsorWithOffers(sponsorId));
		
		//Assert
		//=> exception thrown
	}
	
	
	//Testing postSponsor()
	@Test
	public void should_throw_exception_when_posting_sponsor_with_null_plarform() {
		//Arrange
		SponsorDto sponsor = SponsorDto.builder()
				.platform(null)
				.name("someName")
				.apiURL("someUrl")
				.apiKey("someKey").build();
		boolean refreshSponsorOffers = true;
		
		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		sponsorService.postSponsor(sponsor, refreshSponsorOffers));
		
		//Assert
		//exception thrown
		
	}
	
	@Test
	public void should_throw_exception_when_posting_sponsor_with_null_name() {
		//Arrange
		SponsorDto sponsor = SponsorDto.builder()
				.platform(Platform.HiPath)
				.name(null)
				.apiURL("someUrl")
				.apiKey("someKey").build();
		boolean refreshSponsorOffers = true;
		
		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		sponsorService.postSponsor(sponsor, refreshSponsorOffers));
				
		//Assert
		//exception Thrown
	}
	
	@Test
	public void should_throw_exception_when_posting_sponsor_with_null_apiUrl() {
		//Arrange
		SponsorDto sponsor = SponsorDto.builder()
				.platform(Platform.HiPath)
				.name("someName")
				.apiURL(null)
				.apiKey("someKey").build();
		boolean refreshSponsorOffers = true;
		
		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		sponsorService.postSponsor(sponsor, refreshSponsorOffers));
				
		//Assert
		//exception Thrown
	}
	
	@Test
	public void should_throw_exception_when_posting_sponsor_with_null_apiKey() {
		//Arrange
		SponsorDto sponsor = SponsorDto.builder()
				.platform(Platform.HiPath)
				.name("someName")
				.apiURL("someUrl")
				.apiKey(null).build();
		boolean refreshSponsorOffers = true;
		
		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		sponsorService.postSponsor(sponsor, refreshSponsorOffers));
				
		//Assert
		//exception Thrown
	}
	
	@Test
	public void should_post_sponsor_and_refresh_its_offers() {
		//Arrange
		SponsorDto sponsor = SponsorDto.builder()
				.platform(Platform.HiPath)
				.name("someName")
				.apiURL("someUrl")
				.apiKey("someKey").build();
		boolean refreshSponsorOffers = true;
		when(dataMapper.toEntity(sponsor)).thenReturn(SponsorEntity.builder().build());
		when(sponsorDBRepository.save(any())).thenReturn(SponsorEntity.builder().id(1l).build());
		
		//Act
		sponsorService.postSponsor(sponsor, refreshSponsorOffers);
				
		//Assert
		verify(dataMapper, times(1)).toEntity(sponsor);
		verify(sponsorDBRepository, times(1)).save(any());
		verify(offerService, times(1)).refreshSponsorOffers(any());
		verify(dataMapper, times(1)).toDto(any(SponsorEntity.class));
	}
	
	@Test
	public void should_post_sponsor_without_refreshing_its_offers() {
		//Arrange
		SponsorDto sponsor = SponsorDto.builder()
				.platform(Platform.HiPath)
				.name("someName")
				.apiURL("someUrl")
				.apiKey("someKey").build();
		boolean refreshSponsorOffers = false;
		when(dataMapper.toEntity(sponsor)).thenReturn(SponsorEntity.builder().build());
		when(sponsorDBRepository.save(any())).thenReturn(SponsorEntity.builder().id(1l).build());
		
		//Act
		sponsorService.postSponsor(sponsor, refreshSponsorOffers);
				
		//Assert
		verify(dataMapper, times(1)).toEntity(sponsor);
		verify(sponsorDBRepository, times(1)).save(any());
		verify(offerService, never()).refreshSponsorOffers(any());
		verify(dataMapper, times(1)).toDto(any(SponsorEntity.class));
	}
}
