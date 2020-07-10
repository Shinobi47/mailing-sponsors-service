package com.benayed.mailing.sponsors.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.benayed.mailing.sponsors.entity.OfferEntity;

@Repository
public interface OfferDBRepository extends JpaRepository<OfferEntity, Long>{
	
	public List<OfferEntity> findBySponsor(Long id);
	
	public void deleteBySponsor_id(Long id);
	

}
