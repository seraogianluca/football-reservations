package it.unipi.webserver.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@NoArgsConstructor
public class Message {
    private String username;
    private String message;
    //TODO: timestamp

    public Message(String username, String message) {
        this.username = username;
        this.message = message;
    }
}
