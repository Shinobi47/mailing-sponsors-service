package com.benayed.mailing.sponsors.service;

import static com.benayed.mailing.sponsors.constant.Constants.FALSE;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.springframework.stereotype.Service;

import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.dto.SuppressionInfoDto;
import com.benayed.mailing.sponsors.entity.OfferEntity;
import com.benayed.mailing.sponsors.entity.SponsorEntity;
import com.benayed.mailing.sponsors.enums.Platform;
import com.benayed.mailing.sponsors.exception.ResourceNotFoundException;
import com.benayed.mailing.sponsors.exception.TechnicalException;
import com.benayed.mailing.sponsors.repository.HiPathPlatformRepository;
import com.benayed.mailing.sponsors.repository.OfferDBRepository;
import com.benayed.mailing.sponsors.repository.SponsorDBRepository;
import com.benayed.mailing.sponsors.utils.DataMapper;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Service
@AllArgsConstructor
@Slf4j
public class OfferService {

	private HiPathPlatformRepository hiPathPlatformRepository;
	private SponsorDBRepository sponsorDBRepository;
	private OfferDBRepository offerDBRepository;
	private DataMapper dataMapper;

	public SuppressionInfoDto fetchOfferSuppressionData(Long offerId) {
		OfferEntity offer = offerDBRepository.findById(offerId)
				.orElseThrow(() -> new TechnicalException("No such offer with the given id"));
		
		SponsorEntity sponsor = Optional.ofNullable(offer.getSponsor())
				.orElseThrow(() -> new TechnicalException("The registered offer doesn't have a sponsor !"));
		
		return hiPathPlatformRepository.fetchOfferSuppressionInfo(offer.getCampaignid(), sponsor.getApiKey(), sponsor.getApiURL())
				.orElseThrow(() -> new ResourceNotFoundException("No Suppression data found for the given campaing id !"));
	}
	
	public List<SponsorDto> getAllSponsorsData(){
    	log.info("Fetching all Sponsors");
    	boolean shouldMapOffers = true;
		List<SponsorEntity>  sponsors = sponsorDBRepository.findAll();
		return sponsors.stream().map(sponsor -> dataMapper.toDto(sponsor, shouldMapOffers)).collect(Collectors.toList());
	}
	
	@Transactional
	public void refreshSponsorOffers(Long sponsorId) throws TechnicalException {
		log.info("Refreshing Sponsor offers ...");
		SponsorEntity sponsor = sponsorDBRepository
				.findById(sponsorId)
				.orElseThrow(() -> new TechnicalException("No sponsor found with the sponsor id : " + sponsorId));

		if(Platform.HiPath.equals(sponsor.getPlatform())) {
			refreshSponsorOffersInTheDBFromDistantApi(sponsor);
		}
		else {
			throw new TechnicalException("Unsupported Sponsor Platform. Cannot refresh Offeres.");
		}

	}
	
	public SponsorDto fetchSponsorOffers(String sponsorName) {
    	log.info("Fetching a Sponsor and its Offers...");
    	
    	boolean shouldMapOffers = true;
		return sponsorDBRepository
				.findByName(sponsorName)
				.map(sponsor -> dataMapper.toDto(sponsor, shouldMapOffers))
				.orElseThrow(() -> new ResourceNotFoundException("No sponsor found with the given name"));
	}
	
	private void refreshSponsorOffersInTheDBFromDistantApi(SponsorEntity sponsor) {
		Long timespanInMinutes = 30L; // with HiPath api, we can perform only one call per 30 mins.
		
		if(lastRefreshWasNotInTheLastTimespan(timespanInMinutes, sponsor.getLastOffersRefresh())) {
			List<OfferEntity> freshOffers = hiPathPlatformRepository.fetchOffers(sponsor.getApiKey(), sponsor.getApiURL())
					.stream()
					.map(dataMapper::toEntity)
					.peek(offer -> offer.setSponsor(SponsorEntity.builder().id(sponsor.getId()).build()))
					.peek(offer -> offer.setIsActive(FALSE))
					.collect(Collectors.toList());
			
			persistFreshOffers(sponsor, freshOffers);
		}

	}

	private void persistFreshOffers(SponsorEntity sponsor, List<OfferEntity> freshOffers) {
		sponsor.setLastOffersRefresh(LocalDateTime.now());
		offerDBRepository.deleteBySponsor_id(sponsor.getId());
		sponsorDBRepository.save(sponsor);
		offerDBRepository.saveAll(freshOffers);
	}

	private boolean lastRefreshWasNotInTheLastTimespan(Long timespan, LocalDateTime offersLastRefreshTime) {
		return offersLastRefreshTime == null || LocalDateTime.now().isAfter(offersLastRefreshTime.plusMinutes(timespan));
	}

}
