package com.benayed.mailing.sponsors.dto;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @AllArgsConstructor @NoArgsConstructor @Builder
@XmlRootElement(name = "data")
@XmlAccessorType(XmlAccessType.FIELD)
public class SuppressionDataDto {

	@XmlElement(name = "suppurl")
	private String suppressionDataUrl;
	
	@JsonIgnore
	@XmlElement(name = "supperrmsg")
	private String suppressionErrorMessage;
	
	private String suppressionDataType;
}
