package it.unipi.sqlserver.repository;

import it.unipi.sqlserver.entity.Pitch;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PitchRepository extends JpaRepository<Pitch, Long> {
    Long deletePitchByName(String name);
}
