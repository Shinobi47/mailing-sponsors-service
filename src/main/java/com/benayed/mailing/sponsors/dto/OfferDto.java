package com.benayed.mailing.sponsors.dto;

import java.math.BigDecimal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @AllArgsConstructor @NoArgsConstructor @Builder
@XmlRootElement(name = "data")
@XmlAccessorType(XmlAccessType.FIELD)
public class OfferDto {
	
	private Long id;

	@XmlElement(name = "campaignid")
	private String campaignid;
	
	@XmlElement(name = "name")
	private String name;
	
	@XmlElement(name = "description")
	private String description;
	
	@XmlElement(name = "payout")
	private BigDecimal payout;
	
	@XmlElement(name = "unit")
	private String unit;
	
	@XmlElement(name = "daysleft")
	private Long daysLeft;
	
	@XmlElement(name = "bannercount")
	private Long bannerCount;
	
	@XmlElement(name = "emailcount")
	private Long emailCount;
	
	@XmlElement(name = "textcount")
	private Long textCount;
	
	@XmlElement(name = "category")
	private String category;
	
	@XmlElement(name = "geotargeting")
	private String geoTargeting;
	
	private Boolean isActive;
 
}
