package com.benayed.mailing.sponsors.service;

import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import com.benayed.mailing.sponsors.entity.OfferEntity;
import com.benayed.mailing.sponsors.exception.TechnicalException;
import com.benayed.mailing.sponsors.repository.OfferDBRepository;
import com.benayed.mailing.sponsors.utils.DataMapper;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class OfferActivationService {
	
	private OfferDBRepository offerDBRepository;
	private OfferService offerService;
	private DataMapper dataMapper;
	private RestTemplate restTemplate;
	
	@Value("${assets-service.suppression-url}")
	private String assetsServiceSuppressionUrl;
	
	
	public OfferActivationService(OfferDBRepository offerDBRepository, OfferService offerService, DataMapper dataMapper, RestTemplate restTemplate) {
		
		this.offerDBRepository = offerDBRepository;
		this.offerService = offerService;
		this.dataMapper = dataMapper;
		this.restTemplate = restTemplate;
	}

	public void activateOffer(Long offerId){
		Assert.notNull(offerId, "Cannot activate an offer whose id is NULL !");
		
		log.info("Activating the offer having id : {} ...", offerId);
		OfferEntity offerToActivate = fetchOffer(offerId);
		
		Boolean isOfferAlreadyActive = dataMapper.StringToBoolean(offerToActivate.getIsActive());
		if(isOfferAlreadyActive) {
			throw new TechnicalException("Offer already activate");
		}

		String offerSuppressionDataUrl = offerService
				 .fetchOfferSuppressionData(offerToActivate.getId())
				 .getSuppressionDataUrl();

		postSuppressionInfosToAssetsService(offerToActivate.getId(), offerSuppressionDataUrl);
		 
		offerToActivate.setIsActive(dataMapper.booleanToString(true));
		offerDBRepository.save(offerToActivate);
		
		log.info("Offer with id {} activated successfully !", offerId);

	}

	public void deactivateOffer(Long offerId) {
		Assert.notNull(offerId, "Cannot deactivate an offer whose id is NULL !");

		log.info("Deactivating offer having id : {} ...", offerId);
		OfferEntity offerToDeactivate = fetchOffer(offerId);
		
		Boolean isOfferAlreadyInActive = !dataMapper.StringToBoolean(offerToDeactivate.getIsActive());
		if(isOfferAlreadyInActive) {
			throw new TechnicalException("Offer already deactivated");
		}
		
		deleteSuppressionInfosFromAssetsService(offerId);
		
		offerToDeactivate.setIsActive(dataMapper.booleanToString(false));
		offerDBRepository.save(offerToDeactivate);
		
		log.info("Offer with id {} deactivated successfully !", offerId);
		
	}
	
	private OfferEntity fetchOffer(Long offerId) {
		return offerDBRepository.findById(offerId)
				.orElseThrow(() -> new TechnicalException("No such offer with the given id"));
	}

	private void deleteSuppressionInfosFromAssetsService(Long offerId) {
		try {
			restTemplate.delete(assetsServiceSuppressionUrl + "/" + offerId);
			log.info("Suppression deletiong executed successfully");
		}catch(HttpClientErrorException e) {
			log.error("Exception raised while calling deleting suppression from assets-service...", e);
			throw e;
		}
	}
	private void postSuppressionInfosToAssetsService(Long offerId, String offerSuppressionDataUrl) {
		Assert.notNull(offerSuppressionDataUrl, "SuppressionUrl Must not be null in order to post it to assets service");
		
		HttpEntity<String> httpEntity = buildSuppressionPostRequest(offerId, offerSuppressionDataUrl);

		try {
			ResponseEntity<Void> assetsServiceResponse = restTemplate.postForEntity(assetsServiceSuppressionUrl, httpEntity, Void.class);
			log.info("Suppression posted to assets-service successfully, responce received : {}", assetsServiceResponse);
		}catch(HttpClientErrorException e) {
			log.error("Exception raised while calling posting suppression to assets-service...", e);
			throw e;
		}
	}

	private HttpEntity<String> buildSuppressionPostRequest(Long offerId, String offerSuppressionDataUrl) {
		JSONObject body = new JSONObject();
		body.put("suppressionId", offerId);
		body.put("suppressionLocation", offerSuppressionDataUrl);
		
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);
		
		return new HttpEntity<>(body.toString(), headers);

	}

}
