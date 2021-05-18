package com.benayed.mailing.sponsors.utils;

import static com.benayed.mailing.sponsors.constant.Constants.FALSE;
import static com.benayed.mailing.sponsors.constant.Constants.TRUE;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.springframework.stereotype.Component;

import com.benayed.mailing.sponsors.dto.OfferDto;
import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.entity.OfferEntity;
import com.benayed.mailing.sponsors.entity.SponsorEntity;

@Component
public class DataMapper {
	

	public SponsorDto toDto(SponsorEntity entity) {

		return SponsorDto.builder()
				.id(entity.getId())
				.name(entity.getName())
				.lastOffersRefresh(entity.getLastOffersRefresh())
				.platform(entity.getPlatform()).build();
	}
	
	public SponsorDto toDto(SponsorEntity entity, boolean shouldMapOffers) {
		SponsorDto dto = toDto(entity);
		if(shouldMapOffers) {
			dto.setOffers(mapOffers(entity));
		}
		return dto;
	}
	
	public SponsorEntity toEntity(SponsorDto dto) {
		return SponsorEntity.builder()
				.id(dto.getId())
				.name(dto.getName())
				.platform(dto.getPlatform())
				.lastOffersRefresh(dto.getLastOffersRefresh())
				.apiKey(dto.getApiKey())
				.apiURL(dto.getApiURL()).build();
	}

	public OfferDto toDto(OfferEntity entity) {
		return OfferDto.builder()
				.id(entity.getId())
				.campaignid(entity.getCampaignid())
				.name(entity.getName())
				.description(entity.getDescription())
				.payout(entity.getPayout())
				.unit(entity.getUnit())
				.daysLeft(entity.getDaysLeft())
				.bannerCount(entity.getBannerCount())
				.emailCount(entity.getEmailCount())
				.textCount(entity.getTextCount())
				.category(entity.getCategory())
				.geoTargeting(entity.getGeoTargeting())
				.isActive(StringToBoolean(entity.getIsActive())).build();

	}
	
	public OfferEntity toEntity(OfferDto dto) {
		return OfferEntity.builder()
				.id(dto.getId())
				.campaignid(dto.getCampaignid())
				.name(dto.getName())
				.description(dto.getDescription())
				.payout(dto.getPayout())
				.unit(dto.getUnit())
				.daysLeft(dto.getDaysLeft())
				.bannerCount(dto.getBannerCount())
				.emailCount(dto.getEmailCount())
				.textCount(dto.getTextCount())
				.category(dto.getCategory())
				.geoTargeting(dto.getGeoTargeting())
				.isActive(booleanToString(dto.getIsActive())).build();
	}

	public Boolean StringToBoolean(String isActive) {
		if(TRUE.equals(isActive)) {
			return true;
		}
		else if(FALSE.equals(isActive)) {
			return false;
		}
		return null;
	}
	
	public String booleanToString(Boolean isActive) {
		if(Boolean.TRUE.equals(isActive)) {
			return TRUE;
		}
		else if(Boolean.FALSE.equals(isActive)) {
			return FALSE;
		}
		return null;
	}

	private List<OfferDto> mapOffers(SponsorEntity entity) {
		if(Objects.isNull(entity.getOffers())) {
			return null;
		}
		Function<OfferEntity, OfferDto> toDto = this::toDto;
		return entity.getOffers().stream().map(toDto).collect(Collectors.toList());
	}

}
