package it.unipi.sqlserver.controller;

import it.unipi.sqlserver.entity.Pitch;
import it.unipi.sqlserver.repository.PitchRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.transaction.Transactional;
import java.util.List;

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
        pitchRepository.deletePitchByName(name);
        return "Pitch " + name + " deleted!";
    }

    @GetMapping(path="/read")
    public List<Pitch> browsePitches(){
        List<Pitch> pitches;
        pitches = pitchRepository.findAll();
        return pitches;
    }

    @PutMapping(path="/update/{name}/{status}")
    public Boolean updateStatus(@PathVariable ("status") Boolean status, @PathVariable ("name") String name){
        Pitch pitch;
        pitch = pitchRepository.findPitchByName(name);
        pitch.setAvailable(status);
        pitchRepository.save(pitch);
        return status;
    }
}
