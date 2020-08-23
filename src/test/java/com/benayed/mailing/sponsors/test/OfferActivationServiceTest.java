package com.benayed.mailing.sponsors.test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import com.benayed.mailing.sponsors.constant.Constants;
import com.benayed.mailing.sponsors.dto.SuppressionInfoDto;
import com.benayed.mailing.sponsors.entity.OfferEntity;
import com.benayed.mailing.sponsors.exception.TechnicalException;
import com.benayed.mailing.sponsors.repository.OfferDBRepository;
import com.benayed.mailing.sponsors.service.OfferActivationService;
import com.benayed.mailing.sponsors.service.OfferService;
import com.benayed.mailing.sponsors.utils.DataMapper;

@ExtendWith(MockitoExtension.class)
public class OfferActivationServiceTest {
	
	private OfferActivationService offerActivationService;
	
	@Mock
	private RestTemplate restTemplate;
	
	@Mock
	private OfferService offerService;
	
	@Mock
	private OfferDBRepository offerDBRepository;
	
	private DataMapper dataMapper = new DataMapper();
	
	@BeforeEach
	public void init() {
		
		offerActivationService = new OfferActivationService(offerDBRepository, offerService, dataMapper, restTemplate);
		ReflectionTestUtils.setField(offerActivationService, "assetsServiceSuppressionUrl", "someuUrl");

	}
	
	
	///////////////////////////////   activateOffer TEST CASES   //////////////////////////////////
	@ParameterizedTest
	@NullSource
	public void should_throw_IAException_when_activating_offer_with_null_offerId(Long offerId) {
		//Arrange => method input
		
		//Act
		assertThrows(IllegalArgumentException.class, () ->
		offerActivationService.activateOffer(offerId));
		
		//Assert => exception thrown
		verify(offerDBRepository,never()).save(any());
	}
	
	
	@Test
	public void should_throw_exception_when_no_offer_is_found_for_the_given_id() {
		//Arrange
		Long offerId = 1L;
		when(offerDBRepository.findById(offerId)).thenReturn(Optional.empty());
		
		//Act
		assertThrows(TechnicalException.class, () ->
		offerActivationService.activateOffer(offerId));
		
		//Assert => exception thrown
		verify(offerDBRepository,never()).save(any());
	}
	
	@Test
	public void should_throw_exception_when_offer_is_already_active() {
		//Arrange
		Long offerId = 1L;
		OfferEntity offer =  OfferEntity.builder().isActive(Constants.TRUE).build();
		
		when(offerDBRepository.findById(offerId)).thenReturn(Optional.of(offer));
		
		//Act
		assertThrows(TechnicalException.class, () ->
		offerActivationService.activateOffer(offerId));
		
		//Assert => exception thrown
		verify(offerDBRepository,never()).save(any());

	}
	
	
	@Test
	public void should_throw_exception_when_suppressionUrl_is_Null() {
		//Arrange
		Long offerId = 1L;
		OfferEntity offerInDb =  OfferEntity.builder().id(offerId).isActive(Constants.FALSE).build();
		SuppressionInfoDto suppressionInfoWithNullUrl = SuppressionInfoDto.builder().build();
		
		when(offerDBRepository.findById(offerId)).thenReturn(Optional.of(offerInDb));
		when(offerService.fetchOfferSuppressionData(offerId)).thenReturn(suppressionInfoWithNullUrl);
		//Act
		assertThrows(IllegalArgumentException.class, () ->
		offerActivationService.activateOffer(offerId));
		
		//Assert => exception thrown
		verify(offerDBRepository,never()).save(any());

	}
	
	@Test
	public void should_activate_offer_in_db_when_suppression_post_is_successful() {
		//Arrange
		Long offerId = 1L;
		OfferEntity offerInDb      = OfferEntity.builder().id(offerId).isActive(Constants.FALSE).build();
		OfferEntity offerToBeSaved = OfferEntity.builder().id(offerId).isActive(Constants.TRUE).build();
		
		SuppressionInfoDto suppressionInfo = SuppressionInfoDto.builder().suppressionDataUrl("url").build();
		
		when(offerDBRepository.findById(offerId)).thenReturn(Optional.of(offerInDb));
		when(offerService.fetchOfferSuppressionData(offerId)).thenReturn(suppressionInfo);
		
		//Act
		offerActivationService.activateOffer(offerId);
		
		//Assert
		verify(offerDBRepository, times(1)).save(offerToBeSaved);
		verify(restTemplate, times(1)).postForEntity(anyString(), any(HttpEntity.class), eq(Void.class));
		
	}
	
	
	///////////////////////////////   deactivateOffer TEST CASES   //////////////////////////////////

	@ParameterizedTest
	@NullSource
	public void should_throw_IAException_when_deactivating_offer_with_null_offerId(Long offerId) {
		//Arrange => method input
		
		//Act
		assertThrows(IllegalArgumentException.class, () ->
		offerActivationService.deactivateOffer(offerId));
		
		//Assert => exception thrown
		verify(offerDBRepository,never()).save(any());

	}
	
	@Test
	public void should_throw_exception_when_deactivating_offer_and_no_offer_is_found_for_the_given_id() {
		//Arrange
		Long offerId = 1L;
		when(offerDBRepository.findById(offerId)).thenReturn(Optional.empty());
		
		//Act
		assertThrows(TechnicalException.class, () ->
		offerActivationService.deactivateOffer(offerId));
		
		//Assert => exception thrown
		verify(offerDBRepository,never()).save(any());

	}
	
	@Test
	public void should_throw_exception_when_offer_is_already_deactivated() {
		//Arrange
		Long offerId = 1L;
		OfferEntity offerInDb =  OfferEntity.builder().isActive(Constants.FALSE).build();
		when(offerDBRepository.findById(offerId)).thenReturn(Optional.of(offerInDb));
		
		//Act
		assertThrows(TechnicalException.class, () ->
		offerActivationService.deactivateOffer(offerId));
		
		//Assert => exception thrown
		verify(offerDBRepository,never()).save(any());

	}

	@Test
	public void should_deactivate_offer_in_db_when_suppression_delete_is_successful() {
		//Arrange
		Long offerId = 1L;
		OfferEntity offerInDb =  OfferEntity.builder().id(offerId).isActive(Constants.TRUE).build();
		OfferEntity offerToBeSaved   =  OfferEntity.builder().id(offerId).isActive(Constants.FALSE).build();
				
		when(offerDBRepository.findById(offerId)).thenReturn(Optional.of(offerInDb));
		
		//Act
		offerActivationService.deactivateOffer(offerId);
		
		//Assert
		verify(offerDBRepository, times(1)).save(offerToBeSaved);
		verify(restTemplate, times(1)).delete(anyString());
		
	}
}
