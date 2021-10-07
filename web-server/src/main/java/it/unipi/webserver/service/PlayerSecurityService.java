package it.unipi.webserver.service;

import it.unipi.webserver.entity.Player;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
public class PlayerSecurityService implements UserDetailsService {
    @Autowired
    private SQLDatabase database;

    @Override
    public UserDetails loadUserByUsername(String username)
            throws UsernameNotFoundException {
        Player player = database.getPlayer(username);

        if(player != null) {
            Set<GrantedAuthority> roles = new HashSet<>();
            roles.add(new SimpleGrantedAuthority("PLAYER"));

            List<GrantedAuthority> authorities = new ArrayList<>(roles);

            return new User(player.getUserName(), player.getPassword(), authorities);
        } else throw new UsernameNotFoundException("User " + username + " not found.");
    }
}
