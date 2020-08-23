package com.benayed.mailing.sponsors.exception;

public class TechnicalException extends RuntimeException{

	/**
	 * 
	 */
	private static final long serialVersionUID = 8025495608109560556L;
	
	public TechnicalException() {
		super();
	}
	
	public TechnicalException(String message) {
		super(message);
	}

	public TechnicalException(Exception message) {
		super(message);
	}
}
