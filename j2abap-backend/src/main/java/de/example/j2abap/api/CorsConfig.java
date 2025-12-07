package de.example.j2abap.api;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class CorsConfig implements WebMvcConfigurer {

  @Override
  public void addCorsMappings(CorsRegistry registry) {
    registry.addMapping("/**")
        // Lokal (Vite etc.)
        .allowedOriginPatterns(
            "http://localhost:*",
            "http://127.0.0.1:*",
            // GitHub Pages (Origin ist NUR die Domain – ohne /repo-Pfad)
            "https://chijzay.github.io"
        )
        .allowedMethods("GET", "POST", "OPTIONS")
        .allowedHeaders("*")
        .maxAge(3600);
  }
}
