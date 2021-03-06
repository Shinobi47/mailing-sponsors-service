package com.benayed.mailing.sponsors.repository;

import java.io.StringReader;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.benayed.mailing.sponsors.dto.OfferDatasetDto;
import com.benayed.mailing.sponsors.dto.OfferDto;
import com.benayed.mailing.sponsors.dto.SuppressionInfoDto;
import com.benayed.mailing.sponsors.dto.SuppressionDatasetDto;
import com.benayed.mailing.sponsors.exception.TechnicalException;

import lombok.extern.slf4j.Slf4j;

@Repository
@Slf4j
public class HiPathPlatformRepository {

	private RestTemplate hiPathRestTemplate;
	
	private final String API_GET_CAMPAIGN_FUNC_NAME = "getcampaigns";
	private final String API_GET_SUPPRESSION_FUNC_NAME = "getsuppression";
	private final String API_CAMPAIGNID_PARAM_NAME = "campaignid";
	private final String API_KEY_FIELD_NAME = "apikey";
	private final String API_FUNC_FIELD_NAME = "apifunc";
	
	public HiPathPlatformRepository(RestTemplate hiPathRestTemplate) {
		this.hiPathRestTemplate = hiPathRestTemplate;
	}
	
	public List<OfferDto> fetchOffers(String apiKey, String apiUrl){
		
		Assert.notNull(apiKey, "ApiKey cannot be null");
		Assert.notNull(apiUrl, "ApiUrl cannot be null");
		
		HttpEntity<?> requestEntity = buildHiPathPOSTRequest(apiKey, API_GET_CAMPAIGN_FUNC_NAME);
		
    	log.info("Calling Distant API...");
		ResponseEntity<String> response = this.hiPathRestTemplate.postForEntity(apiUrl, requestEntity, String.class);

		return unmarshallResponse(response, this::unMarshallSponsorOffers);
		
	}
	
	public Optional<SuppressionInfoDto> fetchOfferSuppressionInfo(String campaignId, String apiKey, String apiUrl) {
		
		Assert.notNull(apiKey, "ApiKey cannot be null");
		Assert.notNull(apiUrl, "ApiUrl cannot be null");
		Assert.notNull(campaignId, "Cannot fetch suppression data with null campagn id");
		
    	log.info("Calling Distant API...");
		HttpEntity<MultiValueMap<String, String>> requestEntity = buildHiPathPOSTRequest(apiKey, API_GET_SUPPRESSION_FUNC_NAME);
		requestEntity.getBody().add(API_CAMPAIGNID_PARAM_NAME, campaignId);

		ResponseEntity<String> response = this.hiPathRestTemplate.postForEntity(apiUrl, requestEntity, String.class);
	
		return unmarshallResponse(response, this::unMarshallSponsorSuppressionData).map(this::toDtoWithFormattedSuppressionUrlField);

	}

	private <T> T unmarshallResponse(ResponseEntity<String> response, Function<ResponseEntity<String>, T> unmarshaller){
	    if(isXML(response)) {
	    	log.info("Processing the XML response...");
	    	log.info("Response Content-Type : {}", response.getHeaders().getContentType());
	    	
	    	return unmarshaller.apply(response);
	    }
	    else{
	    	log.warn("The sponsor api responded with a non-XML response");
	    	log.warn("Server response : {}", response.getBody());
	    	
	    	throw new TechnicalException("Cannot fetch offers from sponsor API, error while parsing response data ! body received : " + response.getBody());
	    }
		
	}
	private List<OfferDto> unMarshallSponsorOffers(ResponseEntity<String> response) {
		
		OfferDatasetDto offerDatasetDto = fromStringXmlToObject(OfferDatasetDto.class, response.getBody().toString());
		
		return Optional.ofNullable(offerDatasetDto).map(OfferDatasetDto::getOffers).orElse(null);
	}
	private Optional<SuppressionInfoDto> unMarshallSponsorSuppressionData(ResponseEntity<String> response) {
		SuppressionDatasetDto suppressionDatasetDto = fromStringXmlToObject(SuppressionDatasetDto.class, response.getBody().toString());
		return Optional.ofNullable(suppressionDatasetDto).map(SuppressionDatasetDto::getSuppressionData);
	}
	
	private <T> T fromStringXmlToObject(Class<T> destinationType, String stringXml) {
		T destinationObject = null;
		try (StringReader stringReader = new StringReader(stringXml)){
			JAXBContext jaxbContext = JAXBContext.newInstance(destinationType);
		    Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
		    destinationObject = destinationType.cast(jaxbUnmarshaller.unmarshal(stringReader));

		} catch (JAXBException e) {
			log.error("Error while unmarshalling the response");
			e.printStackTrace();
		}
		return destinationObject;
	}
	
	
	private boolean isXML(ResponseEntity<String> response) {
		return StringUtils.isNotBlank(response.getBody()) && response.getBody().trim().startsWith("<");
	}

	private HttpEntity<MultiValueMap<String, String>> buildHiPathPOSTRequest(String apiKey, String functionToBeCalled) {
		HttpHeaders headers  = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

		MultiValueMap<String, String> body = new LinkedMultiValueMap<String, String>();     
		body.add(API_KEY_FIELD_NAME, apiKey);
		body.add(API_FUNC_FIELD_NAME, functionToBeCalled);
		
		return new HttpEntity<>(body, headers);
	}
	

	private SuppressionInfoDto toDtoWithFormattedSuppressionUrlField(SuppressionInfoDto suppData) {
		// url comes like this form hipath : " url,unknown,unknown
		String extractedSuppDataUrl = suppData.getSuppressionDataUrl() == null ? null :
			suppData.getSuppressionDataUrl().split(",")[0];
		return SuppressionInfoDto.builder()
				.suppressionDataUrl(extractedSuppDataUrl)
				.suppressionErrorMessage(suppData.getSuppressionErrorMessage())
				.build();
	}
}
