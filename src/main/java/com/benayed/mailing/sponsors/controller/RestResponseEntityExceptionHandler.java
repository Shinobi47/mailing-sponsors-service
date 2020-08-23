package com.benayed.mailing.sponsors.controller;

import java.io.IOException;
import java.util.NoSuchElementException;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import com.benayed.mailing.sponsors.exception.TechnicalException;

@RestControllerAdvice
public class RestResponseEntityExceptionHandler extends ResponseEntityExceptionHandler {

	@ResponseBody
	@ExceptionHandler(value = { IllegalArgumentException.class, TechnicalException.class})
	protected void handleBadRequest(RuntimeException e, HttpServletResponse response) throws IOException {
		
		response.sendError(HttpStatus.BAD_REQUEST.value(), e.getMessage());
	}


	@ExceptionHandler(value = {NoSuchElementException.class})
	protected void handleNoSuchElementException(RuntimeException e, HttpServletResponse response) throws IOException {
		
		response.sendError(HttpStatus.NOT_FOUND.value(), e.getMessage());
	}
	
	@ExceptionHandler(value = {HttpClientErrorException.class})
	protected void handleHttpClientErrorException(HttpClientErrorException e,  HttpServletResponse response) throws IOException {
		
		response.sendError(e.getStatusCode().value(), e.getMessage());
	}
	
	@ExceptionHandler(value = {Exception.class})
	protected void handlGenericException(Exception e,  HttpServletResponse response) throws IOException {
		response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value(), "Unexpected error happened, please contact your administrator");
	}
}
	