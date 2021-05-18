package com.benayed.mailing.sponsors.dto;

import java.time.LocalDateTime;
import java.util.List;

import com.benayed.mailing.sponsors.enums.Platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @AllArgsConstructor @NoArgsConstructor @Builder
public class SponsorDto {
	
	private Long id;
	
	private String name;
	
	private Platform platform;
	
	private LocalDateTime lastOffersRefresh;
	
	private String apiURL;
	
	private String apiKey;

	private List<OfferDto> offers;

}
