package com.benayed.mailing.sponsors.dto;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @AllArgsConstructor @NoArgsConstructor
@XmlRootElement(name = "dataset")
@XmlAccessorType(XmlAccessType.FIELD)
public class SuppressionDatasetDto {
	
	@XmlElement(name = "data")
	private SuppressionInfoDto suppressionData;

}
