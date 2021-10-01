package it.unipi.webserver.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;


import java.io.Serializable;


@Getter
@Setter
@ToString
@NoArgsConstructor
public class Pitch implements Serializable {

    private Long id;
    private String name;
    private Boolean available = true;

    public Pitch(String name){
        this.name = name;
    }
}
