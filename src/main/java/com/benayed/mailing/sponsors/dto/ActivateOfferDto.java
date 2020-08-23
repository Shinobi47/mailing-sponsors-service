package com.benayed.mailing.sponsors.dto;

import lombok.Data;

@Data
public class ActivateOfferDto {
	
	private Long offerId;
	private Boolean isActive;

}
