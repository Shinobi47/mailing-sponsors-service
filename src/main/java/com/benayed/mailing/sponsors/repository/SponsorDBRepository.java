package com.benayed.mailing.sponsors.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.benayed.mailing.sponsors.entity.SponsorEntity;

@Repository
public interface SponsorDBRepository extends JpaRepository<SponsorEntity, Long> {
	
	public List<SponsorEntity> findAll();
	
	public Optional<SponsorEntity> findByName(String name);

}
