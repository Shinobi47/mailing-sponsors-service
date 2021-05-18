package com.benayed.mailing.sponsors.controller;

import java.io.IOException;
import java.util.NoSuchElementException;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.client.HttpClientErrorException;

import com.benayed.mailing.sponsors.exception.ResourceNotFoundException;
import com.benayed.mailing.sponsors.exception.TechnicalException;

import lombok.extern.slf4j.Slf4j;

@RestControllerAdvice
@Slf4j
public class RestResponseEntityExceptionHandler{

	@ResponseBody
	@ExceptionHandler(value = { IllegalArgumentException.class, TechnicalException.class})
	protected void handleBadRequest(RuntimeException e, HttpServletResponse response) throws IOException {
		log.error("Exception raised :", e);
		response.sendError(HttpStatus.BAD_REQUEST.value(), e.getMessage());
	}


	@ExceptionHandler(value = {NoSuchElementException.class, ResourceNotFoundException.class})
	protected void handleNoSuchElementException(RuntimeException e, HttpServletResponse response) throws IOException {
		log.error("Exception raised :", e);
		response.sendError(HttpStatus.NOT_FOUND.value(), e.getMessage());
	}
	
	@ExceptionHandler(value = {HttpClientErrorException.class})
	protected void handleHttpClientErrorException(HttpClientErrorException e,  HttpServletResponse response) throws IOException {
		log.error("Exception raised :", e);
		response.sendError(e.getStatusCode().value(), e.getMessage());
	}
	
}
	