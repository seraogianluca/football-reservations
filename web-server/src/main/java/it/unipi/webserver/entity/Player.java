package it.unipi.webserver.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@ToString
@NoArgsConstructor
public class Player implements Serializable {

    private Long playerId;
    private String userName;
    private String password;
    private List<Game> games;

    public Player(String name, String password){
        this.userName = name;
        this.password = password;
    }
}
