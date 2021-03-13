package com.benayed.mailing.sponsors.service;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;

import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.repository.SponsorDBRepository;
import com.benayed.mailing.sponsors.utils.DataMapper;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Service
@AllArgsConstructor
@Slf4j
public class SponsorService {
	
	private SponsorDBRepository sponsorDBRepository;
	private DataMapper dataMapper;
	
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

}
