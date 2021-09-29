package it.unipi.webserver.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.ArrayList;

@Table(name = "player")
@Entity
@Getter
@Setter
@ToString
@NoArgsConstructor
public class Player implements Serializable {
    @Id
    private String username;

    private ArrayList<Reservation> bookings;
}