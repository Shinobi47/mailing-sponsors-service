package com.benayed.mailing.sponsors.controller;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.benayed.mailing.sponsors.dto.SponsorDto;
import com.benayed.mailing.sponsors.dto.SuppressionDataDto;
import com.benayed.mailing.sponsors.service.OfferService;

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
	
	@Operation(summary = "health check opertation")
	@ApiResponses(value = { 
			  @ApiResponse(responseCode = "404", description = "Book not found", 
			    content = @Content) })
	@GetMapping(path = "/hello")
	public String hello() {
		return "Hello world";
	}

	@Operation(summary = "Gets a sponsor and its available offers, if the refresh-offers parameter is true, the API fetches new offers from the distant provider api")
	@ApiResponses(value = { 
			  @ApiResponse(responseCode = "200", description = "Found the sponsor", 
			    content = { @Content(mediaType = "application/json", 
			      schema = @Schema(implementation = SponsorDto.class)) }),
			  @ApiResponse(responseCode = "404", description = "Sponsor not found", 
			    content = @Content) })
	@GetMapping(path = "/sponsor/{name}", produces = "application/json")
	public ResponseEntity<?> fetchSponsorData(@PathVariable String name, @RequestParam(name = "refresh-offers", required = false) Boolean refreshOffers){

		if(Boolean.TRUE.equals(refreshOffers)) {
			offerService.refreshSponsorOffers(name);
		}
		
		SponsorDto sponsor = offerService.fetchSponsorOffers(name);
		return new ResponseEntity<SponsorDto>(sponsor, Objects.nonNull(sponsor) ? HttpStatus.OK : HttpStatus.NOT_FOUND);
	}
	
	@Operation(summary = "Gets suppression data for a specific offer", description = "For this endpoint, only two fields are possible : suppression_data_path and suppression_data_type")
	@ApiResponses(value = { 
			  @ApiResponse(responseCode = "200", description = "Suppression data found", 
			    content = { @Content(mediaType = "application/json", 
			      schema = @Schema(implementation = SponsorDto.class)) }),
			  @ApiResponse(responseCode = "400", description = "Invalid request parameters", 
			    content = @Content) })
	@GetMapping(path = "/offers/{id}", produces = "application/json")
	public ResponseEntity<?> fetch(@PathVariable Long id, @RequestParam List<String> fields){
		
		if(fields.containsAll(Arrays.asList("suppression_data_path","suppression_data_type"))) {
			SuppressionDataDto suppressionData = offerService.fetchOfferSuppressionData(id);
			return new ResponseEntity<SuppressionDataDto>(suppressionData, HttpStatus.OK);
		}
		else {
			return new ResponseEntity<String>("Unsupported requested fieds", HttpStatus.BAD_REQUEST);
		}
	}

}
