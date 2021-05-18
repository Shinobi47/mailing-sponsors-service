package com.benayed.mailing.sponsors.entity;

import java.time.LocalDateTime;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.benayed.mailing.sponsors.enums.Platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data @AllArgsConstructor @NoArgsConstructor @Builder
@Entity
@Table(name = "MAILING_SPONSOR")
public class SponsorEntity {
	
	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE , generator = "spr_id_generator")
	@SequenceGenerator(name="spr_id_generator", sequenceName = "MAILING_SPONSORS_PK_SEQ", allocationSize = 1)
	@Column(name = "SPR_ID")
	private Long id;
	
	@Column(name = "SPR_NAME")
	private String name;
	
	@Column(name = "SPR_LAST_REFRESHED")
	private LocalDateTime lastOffersRefresh;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "SPR_PLATFORM")
	private Platform platform;
	
	@Column(name = "SPR_API_URL")
	private String apiURL;
	
	@Column(name = "SPR_API_KEY")
	private String apiKey;
	
	@OneToMany(mappedBy="sponsor")
	private List<OfferEntity> offers;

}