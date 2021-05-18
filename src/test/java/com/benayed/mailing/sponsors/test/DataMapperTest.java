package com.benayed.mailing.sponsors.test;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Arrays;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import com.benayed.mailing.sponsors.dto.OfferDto;
import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.entity.OfferEntity;
import com.benayed.mailing.sponsors.entity.SponsorEntity;
import com.benayed.mailing.sponsors.enums.Platform;
import com.benayed.mailing.sponsors.utils.DataMapper;

@ExtendWith(MockitoExtension.class)
public class DataMapperTest {

	private DataMapper dataMapper = new DataMapper();
	
	@Test
	public void should_map_primitive_sponsor_entity_fields_except_for_apiurl_and_apikey_and_offers() {
		//Arrange
		SponsorEntity entityToMap = SponsorEntity.builder()
				.id(1l)
				.name("someSponsor")
				.lastOffersRefresh(LocalDateTime.now())
				.platform(Platform.HiPath)
				.apiKey("someKey")
				.apiURL("someUrl")
				.offers(Arrays.asList(OfferEntity.builder().id(1l).build())).build();
		
		//Act
		SponsorDto mappingResult = dataMapper.toDto(entityToMap);
		
		//Assert
		Assertions.assertThat(mappingResult.getId()).isEqualTo(entityToMap.getId());
		Assertions.assertThat(mappingResult.getName()).isEqualTo(entityToMap.getName());
		Assertions.assertThat(mappingResult.getLastOffersRefresh()).isEqualTo(entityToMap.getLastOffersRefresh());
		Assertions.assertThat(mappingResult.getPlatform()).isEqualTo(entityToMap.getPlatform());
		Assertions.assertThat(mappingResult.getApiKey()).isNull();
		Assertions.assertThat(mappingResult.getApiURL()).isNull();
		Assertions.assertThat(mappingResult.getOffers()).isNull();
	}
	
	@Test
	public void should_map_sponsor_without_groups() {
		//Arrange
		boolean shouldMapOffers = false;
		SponsorEntity entityToMap = SponsorEntity.builder()
				.id(1l)
				.name("someSponsor")
				.lastOffersRefresh(LocalDateTime.now())
				.platform(Platform.HiPath)
				.apiKey("someKey")
				.apiURL("someUrl")
				.offers(Arrays.asList(OfferEntity.builder().id(1l).build())).build();
		
		//Act
		SponsorDto mappingResult = dataMapper.toDto(entityToMap, shouldMapOffers);
		
		//Assert
		Assertions.assertThat(mappingResult.getId()).isEqualTo(entityToMap.getId());
		Assertions.assertThat(mappingResult.getName()).isEqualTo(entityToMap.getName());
		Assertions.assertThat(mappingResult.getLastOffersRefresh()).isEqualTo(entityToMap.getLastOffersRefresh());
		Assertions.assertThat(mappingResult.getPlatform()).isEqualTo(entityToMap.getPlatform());
		Assertions.assertThat(mappingResult.getApiKey()).isNull();
		Assertions.assertThat(mappingResult.getApiURL()).isNull();
		Assertions.assertThat(mappingResult.getOffers()).isNull();
	}
	
	@Test
	public void should_map_sponsor_with_offers() {
		//Arrange
		boolean shouldMapOffers = true;
		SponsorEntity entityToMap = SponsorEntity.builder()
				.id(1l)
				.name("someSponsor")
				.lastOffersRefresh(LocalDateTime.now())
				.platform(Platform.HiPath)
				.apiKey("someKey")
				.apiURL("someUrl")
				.offers(Arrays.asList(OfferEntity.builder().id(1l).build())).build();
		
		//Act
		SponsorDto mappingResult = dataMapper.toDto(entityToMap, shouldMapOffers);
		
		//Assert
		Assertions.assertThat(mappingResult.getId()).isEqualTo(entityToMap.getId());
		Assertions.assertThat(mappingResult.getName()).isEqualTo(entityToMap.getName());
		Assertions.assertThat(mappingResult.getLastOffersRefresh()).isEqualTo(entityToMap.getLastOffersRefresh());
		Assertions.assertThat(mappingResult.getPlatform()).isEqualTo(entityToMap.getPlatform());
		Assertions.assertThat(mappingResult.getApiKey()).isNull();
		Assertions.assertThat(mappingResult.getApiURL()).isNull();
		Assertions.assertThat(mappingResult.getOffers()).isNotNull();
	}
	
	
	
	
	
	@Test
	public void should_map_primitive_sponsor_dto_fields_to_entity_except_for_offers() {
		//Arrange
		SponsorDto dtoToMap = SponsorDto.builder()
				.id(1l)
				.name("someSponsor")
				.lastOffersRefresh(LocalDateTime.now())
				.platform(Platform.HiPath)
				.apiKey("someKey")
				.apiURL("someUrl")
				.offers(Arrays.asList(OfferDto.builder().id(1l).build())).build();
		
		//Act
		SponsorEntity mappingResult = dataMapper.toEntity(dtoToMap);
		
		//Assert
		Assertions.assertThat(mappingResult.getId()).isEqualTo(dtoToMap.getId());
		Assertions.assertThat(mappingResult.getName()).isEqualTo(dtoToMap.getName());
		Assertions.assertThat(mappingResult.getLastOffersRefresh()).isEqualTo(dtoToMap.getLastOffersRefresh());
		Assertions.assertThat(mappingResult.getPlatform()).isEqualTo(dtoToMap.getPlatform());
		Assertions.assertThat(mappingResult.getApiKey()).isEqualTo(dtoToMap.getApiKey());
		Assertions.assertThat(mappingResult.getApiURL()).isEqualTo(dtoToMap.getApiURL());
		Assertions.assertThat(mappingResult.getOffers()).isNull();
	}
	
	
	
	@Test
	public void should_map_all_offer_entity_fields_to_dto() {
		//Arrange
		OfferEntity entityToMap = OfferEntity.builder()
				.id(1l)
				.campaignid("someCampaignId")
				.name("someName")
				.description("some description")
				.payout(new BigDecimal(7))
				.unit("some unit")
				.daysLeft(12l)
				.bannerCount(2l)
				.emailCount(3l)
				.textCount(4l)
				.category("some cat")
				.geoTargeting("some get targetting")
				.isActive("1") // 1 == true
				.sponsor(SponsorEntity.builder().id(1l).build())
				.build();
				
		
		//Act
		OfferDto mappingResult = dataMapper.toDto(entityToMap);
		
		//Assert
		Assertions.assertThat(mappingResult.getId()).isEqualTo(entityToMap.getId());
		Assertions.assertThat(mappingResult.getCampaignid()).isEqualTo(entityToMap.getCampaignid());
		Assertions.assertThat(mappingResult.getName()).isEqualTo(entityToMap.getName());
		Assertions.assertThat(mappingResult.getDescription()).isEqualTo(entityToMap.getDescription());
		Assertions.assertThat(mappingResult.getPayout()).isEqualTo(entityToMap.getPayout());
		Assertions.assertThat(mappingResult.getUnit()).isEqualTo(entityToMap.getUnit());
		Assertions.assertThat(mappingResult.getDaysLeft()).isEqualTo(entityToMap.getDaysLeft());
		Assertions.assertThat(mappingResult.getBannerCount()).isEqualTo(entityToMap.getBannerCount());
		Assertions.assertThat(mappingResult.getEmailCount()).isEqualTo(entityToMap.getEmailCount());
		Assertions.assertThat(mappingResult.getTextCount()).isEqualTo(entityToMap.getTextCount());
		Assertions.assertThat(mappingResult.getCategory()).isEqualTo(entityToMap.getCategory());
		Assertions.assertThat(mappingResult.getGeoTargeting()).isEqualTo(entityToMap.getGeoTargeting());
		Assertions.assertThat(mappingResult.getIsActive()).isTrue();
	}
	
	@Test
	public void should_map_all_offer_dto_fields_to_entity() {
		//Arrange
		OfferDto dtoToMap = OfferDto.builder()
				.id(1l)
				.campaignid("someCampaignId")
				.name("someName")
				.description("some description")
				.payout(new BigDecimal(7))
				.unit("some unit")
				.daysLeft(12l)
				.bannerCount(2l)
				.emailCount(3l)
				.textCount(4l)
				.category("some cat")
				.geoTargeting("some get targetting")
				.isActive(true)
				.build();
				
		
		//Act
		OfferEntity mappingResult = dataMapper.toEntity(dtoToMap);
		
		//Assert
		Assertions.assertThat(mappingResult.getId()).isEqualTo(dtoToMap.getId());
		Assertions.assertThat(mappingResult.getCampaignid()).isEqualTo(dtoToMap.getCampaignid());
		Assertions.assertThat(mappingResult.getName()).isEqualTo(dtoToMap.getName());
		Assertions.assertThat(mappingResult.getDescription()).isEqualTo(dtoToMap.getDescription());
		Assertions.assertThat(mappingResult.getPayout()).isEqualTo(dtoToMap.getPayout());
		Assertions.assertThat(mappingResult.getUnit()).isEqualTo(dtoToMap.getUnit());
		Assertions.assertThat(mappingResult.getDaysLeft()).isEqualTo(dtoToMap.getDaysLeft());
		Assertions.assertThat(mappingResult.getBannerCount()).isEqualTo(dtoToMap.getBannerCount());
		Assertions.assertThat(mappingResult.getEmailCount()).isEqualTo(dtoToMap.getEmailCount());
		Assertions.assertThat(mappingResult.getTextCount()).isEqualTo(dtoToMap.getTextCount());
		Assertions.assertThat(mappingResult.getCategory()).isEqualTo(dtoToMap.getCategory());
		Assertions.assertThat(mappingResult.getGeoTargeting()).isEqualTo(dtoToMap.getGeoTargeting());
		Assertions.assertThat(mappingResult.getIsActive()).isEqualTo("1"); // 1 == true
	}
	
	
	
	
	
	
	
	
	@Test
	public void should_not_map_string_to_boolean_if_unknown_value() {
		//Arrange
		String active = "neither 0 nor 1";
		
		//Act
		Boolean isActive = dataMapper.StringToBoolean(active);
		
		//Assert
		Assertions.assertThat(isActive).isNull();;
	}
	
	@Test
	public void should_map_1_to_true() {
		//Arrange
		String active = "1";
		
		//Act
		Boolean isActive = dataMapper.StringToBoolean(active);
		
		//Assert
		Assertions.assertThat(isActive).isTrue();
	}
	
	@Test
	public void should_map_0_to_false() {
		//Arrange
		String inactive = "0";
		
		//Act
		Boolean isActive = dataMapper.StringToBoolean(inactive);
		
		//Assert
		Assertions.assertThat(isActive).isFalse();
	}
	
	
	@Test
	public void should_not_map_boolean_to_string_if_null_Boolean() {
		//Arrange
		Boolean active = null;
		
		//Act
		String isActive = dataMapper.booleanToString(active);
		
		//Assert
		Assertions.assertThat(isActive).isNull();;
	}
	
	@Test
	public void should_map_true_to_1() {
		//Arrange
		Boolean active = true;
		
		//Act
		String isActive = dataMapper.booleanToString(active);
		
		//Assert
		Assertions.assertThat(isActive).isEqualTo("1");
	}
	
	@Test
	public void should_map_false_to_0() {
		//Arrange
		Boolean inactive = false;
		
		//Act
		String isActive = dataMapper.booleanToString(inactive);
		
		//Assert
		Assertions.assertThat(isActive).isEqualTo("0");
	}
}

