package com.benayed.mailing.sponsors.test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.benayed.mailing.sponsors.dto.OfferDto;
import com.benayed.mailing.sponsors.dto.SuppressionInfoDto;
import com.benayed.mailing.sponsors.exception.TechnicalException;
import com.benayed.mailing.sponsors.repository.HiPathPlatformRepository;


@ExtendWith(MockitoExtension.class)
class HiPathRepositoryTests {

	@Mock
	private RestTemplate restTemplate;
	
	private HiPathPlatformRepository hiPathPlatformRepository;
	
	@Captor
	private ArgumentCaptor<HttpEntity<MultiValueMap<String, String>>> hiPathRequestCaptor;
	
	@BeforeEach
	public void init() {
		hiPathPlatformRepository = new HiPathPlatformRepository(restTemplate);
	}
	
	
	/////////////////////////////////////  Testing fetchOffers
	@Test
	public void should_parse_offer_correctly_to_object_when_hipath_api_is_called(){
		
		//Arrange
		String campaignId = "3885";
		String description = "All display placements need written approval prior to going live.";
		String name = "FreeCreditClick - Display Only";
		BigDecimal payout = new BigDecimal("32.01");
		String unit = "per sale";
		Long emailCount = 1L;
		Long daysLeft = 7L;
		Long bannerCount = 2L;
		Long textCount = 3L;
		String category = "misc";
		String  geoTargeting = "All traffic accepted";
		
		String response = buildXmlOffer(campaignId, description, name, payout.doubleValue(), unit, emailCount, daysLeft, bannerCount, textCount,
				category, geoTargeting);
		when(restTemplate.postForEntity(anyString(), any(), any())).thenReturn(new ResponseEntity<>(response, HttpStatus.OK));
		
		
		//Act
		List<OfferDto> offers = hiPathPlatformRepository.fetchOffers("an api key", "an api url");
		
		//Assert
		assertThat(offers).hasSize(1);
		OfferDto responseOffer = offers.get(0);
		assertThat(responseOffer.getCampaignid()).isEqualTo(campaignId);
		assertThat(responseOffer.getDescription()).isEqualTo(description);
		assertThat(responseOffer.getPayout().compareTo(payout)).isEqualTo(0); //equals
		assertThat(responseOffer.getUnit()).isEqualTo(unit);
		assertThat(responseOffer.getDaysLeft()).isEqualTo(daysLeft);
		assertThat(responseOffer.getBannerCount()).isEqualTo(bannerCount);
		assertThat(responseOffer.getEmailCount()).isEqualTo(emailCount);
		assertThat(responseOffer.getTextCount()).isEqualTo(textCount);
		assertThat(responseOffer.getCategory()).isEqualTo(category);
		assertThat(responseOffer.getGeoTargeting()).isEqualTo(geoTargeting);
		
	}
	@Test
	public void should() {
		//Arrange
		String apiKey = "someApiKey";
		String apiUrl = "someApiUrl";
		String apiFuncToCall = "getcampaigns";

		String response = buildXmlSuppressionData("someSuppressMsg", "someSuppressUrl");
		when(restTemplate.postForEntity(anyString(), any(), any())).thenReturn(new ResponseEntity<>(response, HttpStatus.OK));
		
		//Act
		hiPathPlatformRepository.fetchOffers(apiKey, apiUrl);
		
		//Assert
		verify(restTemplate, times(1)).postForEntity(eq(apiUrl), hiPathRequestCaptor.capture(), eq(String.class));
		HttpEntity<MultiValueMap<String, String>> request = hiPathRequestCaptor.getValue();
		
		MultiValueMap<String, String> body = request.getBody();
		assertThat(body).hasSize(2);
		assertThat(body.get("apikey").get(0)).isEqualTo(apiKey);
		assertThat(body.get("apifunc").get(0)).isEqualTo(apiFuncToCall);
		
		HttpHeaders headers = request.getHeaders();
		assertThat(headers).hasSize(1);
		assertThat(headers.getContentType()).isEqualTo(MediaType.APPLICATION_FORM_URLENCODED);
	}

	@Test
	public void should_throw_exception_when_calling_hipathApi_with_null_apiKey() {
		//Arrange
		String apiKey = null;
		String apiUrl = "anApiUrl";
		
		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		hiPathPlatformRepository.fetchOffers(apiKey, apiUrl));
		
		//Assert
		//=> Exception thrown
	}
	
	@Test
	public void should_throw_exception_when_calling_hipathApi_with_null_apiUrl() {
		//Arrange
		String apiKey = "anApiKey";
		String apiUrl = null;
		
		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		hiPathPlatformRepository.fetchOffers(apiKey, apiUrl));
		
		//Assert
		//=> Exception thrown
	}
	
	
	
	///////////////////////////////////////   Testing fetchOfferSuppressionInfo
	@Test
	public void should_parse_suppression_data_to_object_correctly_when_hipath_api_is_called() {
		//Arrange
		String suppressUrl = "url1";
		String suppressMsg = "msg1";

		String response = buildXmlSuppressionData(suppressMsg, suppressUrl);
		when(restTemplate.postForEntity(anyString(), any(), any())).thenReturn(new ResponseEntity<>(response, HttpStatus.OK));

		
		//Act
		Optional<SuppressionInfoDto> suppressionData = hiPathPlatformRepository.fetchOfferSuppressionInfo("campaignId1", "someKey", "someUrl");
		
		//Assert
		assertThat(suppressionData).isPresent();
		assertThat(suppressionData.get().getSuppressionDataUrl()).isEqualTo(suppressUrl);
		assertThat(suppressionData.get().getSuppressionErrorMessage()).isEqualTo(suppressMsg);
	}
	
	@Test
	public void should_throw_Exception_when_fetching_suppression_data_with_null_api_url() {
		//Arrange
		String apiKey = "anApiKey";
		String apiUrl = null;
		String campaignId = "id1";

		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		hiPathPlatformRepository.fetchOfferSuppressionInfo(campaignId, apiKey, apiUrl));
		
		//Assert
		//=> exception thrown
	}
	
	
	@Test
	public void should_throw_Exception_when_fetching_suppression_data_with_null_api_key() {
		//Arrange
		String apiKey = null;
		String apiUrl = "anApiUrl";
		String campaignId = "id1";

		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		hiPathPlatformRepository.fetchOfferSuppressionInfo(campaignId, apiKey, apiUrl));
		
		//Assert
		//=> exception thrown
	}
	
	
	@Test
	public void should_throw_Exception_when_fetching_suppression_data_with_null_campaign_id() {
		//Arrange
		String apiKey = "anApiKey";
		String apiUrl = "anApiUrl";
		String campaignId = null;

		//Act
		Assertions.assertThrows(IllegalArgumentException.class, () -> 
		hiPathPlatformRepository.fetchOfferSuppressionInfo(campaignId, apiKey, apiUrl));
		
		//Assert
		//=> exception thrown
	}
	
	
	@Test
	public void should_call_hiPath_api_with_right_body_and_headers_when_fetching_suppression_offer() {
		//Arrange
		String apiUrl = "someUrl";
		String apiKey = "someKey";
		String campaignId = "someId";
		String apiFuncToCall = "getsuppression";

		String response = buildXmlSuppressionData("someSuppressMsg", "someSuppressUrl");
		when(restTemplate.postForEntity(anyString(), any(), any())).thenReturn(new ResponseEntity<>(response, HttpStatus.OK));

		
		//Act
		hiPathPlatformRepository.fetchOfferSuppressionInfo(campaignId, apiKey, apiUrl);
		
		//Assert
		verify(restTemplate, times(1)).postForEntity(eq(apiUrl), hiPathRequestCaptor.capture(), eq(String.class));
		HttpEntity<MultiValueMap<String, String>> request = hiPathRequestCaptor.getValue();
		
		MultiValueMap<String, String> body = request.getBody();
		assertThat(body).hasSize(3);
		assertThat(body.get("apikey").get(0)).isEqualTo(apiKey);
		assertThat(body.get("apifunc").get(0)).isEqualTo(apiFuncToCall);
		assertThat(body.get("campaignid").get(0)).isEqualTo(campaignId);
		
		HttpHeaders headers = request.getHeaders();
		assertThat(headers).hasSize(1);
		assertThat(headers.getContentType()).isEqualTo(MediaType.APPLICATION_FORM_URLENCODED);
	}
	
	
	@ParameterizedTest
	@NullAndEmptySource
	@ValueSource(strings = "not an xml")
	public void should_throw_exception_if_hiPath_response_is_not_XML_when_fetching_suppression_offer(String responseThatIsNotXml) {
		//Arrange
		String apiUrl = "someUrl";
		String apiKey = "someKey";
		String campaignId = "someId";

		when(restTemplate.postForEntity(anyString(), any(), any())).thenReturn(new ResponseEntity<>(responseThatIsNotXml, HttpStatus.OK));

		
		//Act
		Assertions.assertThrows(TechnicalException.class, () -> 
		hiPathPlatformRepository.fetchOfferSuppressionInfo(campaignId, apiKey, apiUrl));
		
		//Assert
		//Exception raised

	}
	

	
	
	
	
	private String buildXmlSuppressionData(String suppressMsg, String suppressUrl) {
		return "<?xml version='1.0' encoding='UTF-8'?>" + 
				"<dataset apifunc='getsuppression' campaignid='3885'>" + 
				"	<data>\r\n" + 
				"		<suppurl><![CDATA[" + suppressUrl + ",,]]></suppurl>" + 
				"		<supperrmsg><![CDATA[" + suppressMsg + "]]></supperrmsg>" + 
				"	</data>\r\n" + 
				"</dataset>";
	}
	private String buildXmlOffer(String campaignId, String description, String name, Double payout, String unit,
			Long emailCount, Long daysLeft, Long bannerCount, Long textCount, String category, String geoTargeting) {
		return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + 
				"<dataset apifunc=\"getcampaigns\">" + 
				"   <data>" + 
				"      <campaignid><![CDATA[" + campaignId + "]]></campaignid>" + 
				"      <name><![CDATA["+ name +"]]></name>" + 
				"      <description><![CDATA["+ description +"]]></description>" + 
				"      <payout><![CDATA[" + payout + "]]></payout>" + 
				"      <unit><![CDATA[" + unit + "]]></unit>" + 
				"      <daysleft><![CDATA[" + daysLeft + "]]></daysleft>" + 
				"      <bannercount><![CDATA[" + bannerCount + "]]></bannercount>" + 
				"      <emailcount><![CDATA[" + emailCount + "]]></emailcount> " + 
				"      <textcount><![CDATA[" + textCount + "]]></textcount>" + 
				"      <category><![CDATA[" + category + "]]></category>" + 
				"      <geotargeting><![CDATA[" + geoTargeting + "]]></geotargeting>" + 
				"   </data>" +
				"</dataset>";
	}

}
