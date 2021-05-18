package com.benayed.mailing.sponsors.config;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.http.MediaType;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;

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
	
	@Bean
	public WebMvcConfigurer corsConfigurer() {
		return new WebMvcConfigurer() {
			@Override
			public void addCorsMappings(CorsRegistry registry) {
				registry.addMapping("/**").allowedOrigins("*");
			}
		};
	}
	
	
	@Bean
	@Primary
    public ObjectMapper buildObjectMapper() {
		ObjectMapper objectMapper = new ObjectMapper();
		objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
		objectMapper.registerModule(new SimpleModule().addSerializer(LocalDateTime.class, LocalDateTimeSerializer.INSTANCE));
		objectMapper.registerModule(new SimpleModule().addDeserializer(LocalDateTime.class, LocalDateTimeDeserializer.INSTANCE));
		objectMapper.setSerializationInclusion(Include.NON_NULL);
		return objectMapper;
    }
}
