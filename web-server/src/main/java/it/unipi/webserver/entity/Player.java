package it.unipi.webserver.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;

@Entity
@Getter
@Setter
@NoArgsConstructor
public class Player implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long playerId;

    private String userName;
    private String password;

    @ManyToMany(mappedBy = "players", fetch = FetchType.EAGER)
    @JsonBackReference
    private List<Game> games;

    @OneToMany(mappedBy = "player",cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Notice> notices;

    public Player(String name, String password){
        this.userName = name;
        this.password = password;
    }
}
