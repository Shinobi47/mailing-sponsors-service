package com.benayed.mailing.sponsors.config;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.web.client.RestTemplate;

@Configuration
public class AppConfig {
	
	@Bean //bean qualifier is method name
	public RestTemplate hiPathRestTemplate() {
		RestTemplate restTemplate = new RestTemplate();
	
		StringHttpMessageConverter stringResponseUnmarshallingConverter = new StringHttpMessageConverter();
		stringResponseUnmarshallingConverter.setSupportedMediaTypes(Arrays.asList(MediaType.TEXT_HTML, MediaType.TEXT_XML));
		
		FormHttpMessageConverter postRequestMarshallingConverter = new FormHttpMessageConverter();
		postRequestMarshallingConverter.setSupportedMediaTypes(Collections.singletonList(MediaType.APPLICATION_FORM_URLENCODED));

		List<HttpMessageConverter<?>> messageConverters = new ArrayList<HttpMessageConverter<?>>();
		messageConverters.add(stringResponseUnmarshallingConverter);
		messageConverters.add(postRequestMarshallingConverter);
				
		restTemplate.setMessageConverters(messageConverters); 
	    return restTemplate;
	}
	
	@Bean
	public RestTemplate restTemplate() {
		return new RestTemplate();
	}
	
}
