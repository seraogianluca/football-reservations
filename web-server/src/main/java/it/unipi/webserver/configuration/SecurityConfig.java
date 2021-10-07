package it.unipi.webserver.configuration;

import it.unipi.webserver.service.PlayerSecurityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.servlet.PathRequest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.NoOpPasswordEncoder;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

@Configuration
@EnableWebSecurity
public class SecurityConfig extends WebSecurityConfigurerAdapter {
    @Autowired
    LoginSuccess loginSuccess;
    @Autowired
    PlayerSecurityService playersManager;

    @Bean
    public UserDetailsService myUserDetails() {
        return new PlayerSecurityService();
    }

    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {
        UserDetailsService userDetailsService = myUserDetails();

        // For simplicity disable password encoding
        auth.userDetailsService(playersManager).passwordEncoder(NoOpPasswordEncoder.getInstance());
    }

    @Override
    public void configure(HttpSecurity http) throws Exception {
        http.authorizeRequests()
                .requestMatchers(PathRequest.toStaticResources().atCommonLocations()).permitAll()
                .antMatchers("/", "/login").permitAll()
                .antMatchers("/home/**").hasAnyAuthority("PLAYER")
                .anyRequest().authenticated()
                    .and()
                .formLogin()
                    .loginPage("/login")
                    .failureUrl("/login/?error")
                    .successHandler(loginSuccess)
                    .permitAll()
                    .and()
                .logout().permitAll(); // Permit get logout for simplicity
                //    .logoutRequestMatcher(new AntPathRequestMatcher("/logout"))
                //    .logoutSuccessUrl("/login/?logout");
    }
}
