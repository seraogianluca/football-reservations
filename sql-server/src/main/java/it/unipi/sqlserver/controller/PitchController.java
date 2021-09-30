package it.unipi.sqlserver.controller;

import it.unipi.sqlserver.entity.Pitch;
import it.unipi.sqlserver.repository.PitchRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.transaction.Transactional;

@RestController
@RequestMapping("pitch")
public class PitchController {
    @Autowired
    private PitchRepository pitchRepository;

    @PostMapping(path = "/add/{name}")
    public String addPitch(@PathVariable("name") String name) {
        //TODO:check for unique pitch name
        Pitch pitch = new Pitch();
        pitch.setName(name);
        pitchRepository.save(pitch);
        return "Saved";
    }

    @DeleteMapping(path= "/delete/{name}")
    @Transactional
    public String deletePitch(@PathVariable ("name") String name){
        Long id;
        id = pitchRepository.deletePitchByName(name);
        return "Pitch id:" + id + " name:" + name + " deleted!";
    }
}
