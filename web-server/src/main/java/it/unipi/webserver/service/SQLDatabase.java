package it.unipi.webserver.service;

import it.unipi.webserver.entity.Pitch;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
public class SQLDatabase {

    private String baseUri = "http://localhost:8080";
    @Autowired
    private WebClient.Builder webClientBuilder;


    @GetMapping(path ="/read")
    public List<Pitch> browsePitches(){
        return this.webClientBuilder.build()
                .get()
                .uri(baseUri + "/pitch/read")
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<List<Pitch>>() {
                })
                .block();

    }
    @PostMapping(path="/add/{name}")
    public String addPitch(@PathVariable("name") String name){
        return this.webClientBuilder.build()
                .post()
                .uri(baseUri + "/pitch/add/" + name)
                .retrieve()
                .bodyToMono(String.class)
                .block();
    }

    @DeleteMapping(path="/delete/{name}")
    public String deletePitch(@PathVariable("name") String name){
        return this.webClientBuilder.build()
                .delete()
                .uri(baseUri + "/pitch/delete/" + name)
                .retrieve()
                .bodyToMono(String.class)
                .block();
    }

    @PutMapping(path="/update/{name}/{status}")
    public Boolean updateStatus(@PathVariable ("status") Boolean status
            , @PathVariable ("name") String name){
        return this.webClientBuilder.build()
                .put()
                .uri(baseUri + "/pitch/update/" + name + "/" + status)
                .retrieve()
                .bodyToMono(Boolean.class)
                .block();
    }
}
