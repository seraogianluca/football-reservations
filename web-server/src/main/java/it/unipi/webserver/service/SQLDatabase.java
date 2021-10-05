package it.unipi.webserver.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

@Service
public class SQLDatabase {

    private final WebClient database;

    @Autowired
    public SQLDatabase() {
        this.database = WebClient.builder().baseUrl("http://localhost:8080/").build();
    }

    //Add player
    //Add match
    //Book game

    /* ADD MATCH */
    public String addGame(String playerManager, String pitchName, int time) {
        String requestPath = playerManager + "/" + pitchName + "/" + time;
        return database.post()
                .uri("game/add/" + requestPath)
                .retrieve()
                .bodyToMono(String.class)
                .block();
    }

    /*
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
     */
    /*
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
    */
}
