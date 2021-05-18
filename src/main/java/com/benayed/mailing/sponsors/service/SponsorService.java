package com.benayed.mailing.sponsors.service;

import java.util.List;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.entity.SponsorEntity;
import com.benayed.mailing.sponsors.exception.ResourceNotFoundException;
import com.benayed.mailing.sponsors.repository.OfferDBRepository;
import com.benayed.mailing.sponsors.repository.SponsorDBRepository;
import com.benayed.mailing.sponsors.utils.DataMapper;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Service
@AllArgsConstructor
@Slf4j
public class SponsorService {
	
	private SponsorDBRepository sponsorDBRepository;
	private OfferDBRepository offerDBRepository;
	private DataMapper dataMapper;
	private OfferService offerService;
	
	
	public List<SponsorDto> fetchSponsors(){
		log.info("Fetching sponsors ...");
		
		return this.sponsorDBRepository
				.findAll().stream()
				.map(dataMapper::toDto)
				.collect(Collectors.toList());
	}

	public List<SponsorDto> fetchSponsorsWithOffers(){
		log.info("Fetching sponsors with their offers...");
		
		boolean shouldMapOffers = true;
		return this.sponsorDBRepository
				.findAll().stream()
				.map(sponsorEntity -> dataMapper.toDto(sponsorEntity, shouldMapOffers))
				.collect(Collectors.toList());
	}
	
	@Transactional
	public SponsorDto postSponsor(SponsorDto sponsor, boolean refreshSponsorOffers) {
		Assert.notNull(sponsor.getPlatform(), "Cannot post a sponsor with Null Platform");
		Assert.notNull(sponsor.getName(), "Cannot post a sponsor with Null Name");
		Assert.notNull(sponsor.getApiURL(), "Cannot post a sponsor with Null  apiUrl");
		Assert.notNull(sponsor.getApiKey(), "Cannot post a sponsor with Null apiKey");

		SponsorEntity sponsorEntity = this.sponsorDBRepository.save(this.dataMapper.toEntity(sponsor));

		if(refreshSponsorOffers) {
			offerService.refreshSponsorOffers(sponsorEntity.getId());
		}
		return this.dataMapper.toDto(sponsorEntity);
	
	}
	
	@Transactional
	public void deleteSponsor(Long id) {
		offerDBRepository.deleteBySponsor_id(id);
		sponsorDBRepository.deleteById(id);
	}
	
	
	public SponsorDto fetchSponsorWithOffers(Long sponsorId) {
		return sponsorDBRepository.findById(sponsorId)
		.map(entity -> dataMapper.toDto(entity, true))
		.orElseThrow(() -> new ResourceNotFoundException("no sponsor found with id : " + sponsorId));
	}

}
