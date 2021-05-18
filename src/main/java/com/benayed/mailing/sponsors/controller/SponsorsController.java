package com.benayed.mailing.sponsors.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.benayed.mailing.sponsors.dto.ActivateOfferDto;
import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.service.OfferActivationService;
import com.benayed.mailing.sponsors.service.OfferService;
import com.benayed.mailing.sponsors.service.SponsorService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.AllArgsConstructor;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1")
public class SponsorsController {
	
	private OfferService offerService;
	private OfferActivationService offerActivationService;
	private SponsorService sponsorService;
	
	@Operation(summary = "health check opertation")
	@ApiResponses(value = { 
			  @ApiResponse(responseCode = "404", description = "Book not found", 
			    content = @Content) })
	@GetMapping(path = "/hello")
	public String hello() {
		return "Hello world";
	}

	@Operation(summary = "activate or deactivate an offer", description = "The patch supported ATM is to activate or deactivate an offer by providing a json input containing the fields 'offerId' and 'isActive'")
	@ApiResponses(value = { 
			  @ApiResponse(responseCode = "200", description = "Offer patched successfully"),
			  @ApiResponse(responseCode = "400", description = "Invalid request parameters", 
			    content = @Content) })
	@PatchMapping(path = "/offers/{id}")
	public void patch(@PathVariable(name = "id")Long offerId, @RequestBody ActivateOfferDto offerPatch) {
		
		if(Boolean.TRUE.equals(offerPatch.getIsActive())) {
			offerActivationService.activateOffer(offerId);
		}
		
		else if(Boolean.FALSE.equals(offerPatch.getIsActive())) {
			offerActivationService.deactivateOffer(offerId);
		}
	
		throw new IllegalArgumentException("Unsupported patch operation");
	
	}
	
	
	@Operation(summary = "Get all available sponsors list")
	@ApiResponses(value = { 
			  @ApiResponse(responseCode = "200", description = "Found the sponsors", 
			    content = { @Content(mediaType = "application/json", 
			      schema = @Schema(implementation = SponsorDto.class)) }),
			  @ApiResponse(responseCode = "404", description = "no sponsors found", 
			    content = @Content) })
	@GetMapping(path = "/sponsors/", produces = "application/json")
	public ResponseEntity<?> fetchSponspors(){

		List<SponsorDto> sponsors = sponsorService.fetchSponsors();
		return CollectionUtils.isEmpty(sponsors)
				? new ResponseEntity<>(HttpStatus.NOT_FOUND)
						: new ResponseEntity<List<SponsorDto>>(sponsors, HttpStatus.OK);
	}
	
	
	@Operation(summary = "Get all available sponsors list with their offers")
	@ApiResponses(value = {
			  @ApiResponse(responseCode = "200", description = "Found the sponsors", 
			    content = { @Content(mediaType = "application/json", 
			      schema = @Schema(implementation = SponsorDto.class)) }),
			  @ApiResponse(responseCode = "404", description = "no sponsors found", 
			    content = @Content) })
	@GetMapping(path = "/sponsors/offers", produces = "application/json")
	public ResponseEntity<?> fetchSponsporsWithOffers(){

		List<SponsorDto> sponsors = sponsorService.fetchSponsorsWithOffers();
		return CollectionUtils.isEmpty(sponsors)
				? new ResponseEntity<>(HttpStatus.NOT_FOUND)
						: new ResponseEntity<List<SponsorDto>>(sponsors, HttpStatus.OK);
	}
	
	
	@Operation(summary = "Add a sponsor only name, platform, apikey, apiUrl fields are to be posted")
	@ApiResponses(value = {
			  @ApiResponse(responseCode = "200", description = "Sponsor added succesfully", 
			    content = { @Content(mediaType = "application/json", 
			      schema = @Schema(implementation = SponsorDto.class)) })
			  })
	@PostMapping(path = "/sponsors", consumes = "application/json", produces = "application/json")
	public ResponseEntity<SponsorDto> addSponsor(@RequestBody SponsorDto sponsor, @RequestParam(defaultValue = "false") boolean refreshSponsorOffers){

		SponsorDto postedSponsor = this.sponsorService.postSponsor(sponsor, refreshSponsorOffers);
		return new ResponseEntity<SponsorDto>(postedSponsor, HttpStatus.OK);
	}
	

	@Operation(summary = "Delete a sponsor by its id")
	@ApiResponses(value = {
			  @ApiResponse(responseCode = "200", description = "Sponsor deleted succesfully")
			  })
	@DeleteMapping(path = "/sponsors/{id}")
	public ResponseEntity<?> deleteSponsor(@PathVariable(name = "id") Long sponsorId){

		this.sponsorService.deleteSponsor(sponsorId);
		return new ResponseEntity<>(HttpStatus.OK);
	}
	
	@Operation(summary = "activate or deactivate an offer", description = "The patch supported ATM is to activate or deactivate an offer by providing a json input containing the fields 'offerId' and 'isActive'")
	@ApiResponses(value = { 
			  @ApiResponse(responseCode = "200", description = "Offer patched successfully"),
			  @ApiResponse(responseCode = "400", description = "Invalid request parameters", 
			    content = @Content) })
	@PatchMapping(path = "/sponsors/{id}")
	public SponsorDto patch(@PathVariable(name = "id") Long sponsorId, @RequestBody HashMap<String, String> patch) {
		
		Optional.ofNullable(patch)
			.map(p -> p.get("action"))
			.filter("refresh"::equalsIgnoreCase)
			.orElseThrow(() -> new IllegalArgumentException("Unsupported patch operation"));
		
		this.offerService.refreshSponsorOffers(sponsorId);
		return this.sponsorService.fetchSponsorWithOffers(sponsorId);
	
	}
	
}
