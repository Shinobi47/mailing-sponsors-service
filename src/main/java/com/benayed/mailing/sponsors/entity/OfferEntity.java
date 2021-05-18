package com.benayed.mailing.sponsors.entity;

import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @AllArgsConstructor @NoArgsConstructor @Builder
@Entity
@Table(name = "MAILING_OFFERS")
public class OfferEntity {

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE , generator = "ofr_id_generator")
	@SequenceGenerator(name="ofr_id_generator", sequenceName = "MAILING_OFFERS_PK_SEQ", allocationSize = 1)
	@Column(name = "OFR_ID")
	private Long id;
	
	@Column(name = "OFR_CMP_ID")
	private String campaignid;
	
	@Column(name = "OFR_NAME")
	private String name;
	
	@Column(name = "OFR_DESC")
	private String description;
	
	@Column(name = "OFR_PAYOUT")
	private BigDecimal payout;
	
	@Column(name = "OFR_UNIT")
	private String unit;
	
	@Column(name = "OFR_DAYS_LEFT")
	private Long daysLeft;
	
	@Column(name = "OFR_BNR_CNT")
	private Long bannerCount;
	
	@Column(name = "OFR_MAIL_CNT")
	private Long emailCount;
	
	@Column(name = "OFR_TXT_CNT")
	private Long textCount;
	
	@Column(name = "OFR_CAT")
	private String category;
	
	@Column(name = "OFR_GEO_TRG")
	private String geoTargeting;
	
	@Column(name = "OFR_IS_ACTIVE")
	private String isActive;
	
	@ManyToOne
    @JoinColumn(name="OFR_SPR_ID")
	private SponsorEntity sponsor;

	@Override
	public String toString() {
		return "OfferEntity [id=" + id + ", campaignid=" + campaignid + ", name=" + name + ", description="
				+ description + ", payout=" + payout + ", unit=" + unit + ", daysLeft=" + daysLeft + ", bannerCount="
				+ bannerCount + ", emailCount=" + emailCount + ", textCount=" + textCount + ", category=" + category
				+ ", geoTargeting=" + geoTargeting + ", isActive=" + isActive + ", sponsorId=" + sponsor.getId() + "]";
	}
	
	
}


