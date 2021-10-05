package it.unipi.sqlserver.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;

@Entity
@Getter
@Setter
@ToString
@NoArgsConstructor
public class Player implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long playerId;
    private String userName;
    private String password;
    @ManyToMany(mappedBy = "players", fetch = FetchType.EAGER)
    private List<Game> games;

    public Player(String name, String password){
        this.userName = name;
        this.password = password;

    }
}
