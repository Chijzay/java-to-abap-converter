package de.example.j2abap.api;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class CorsConfig implements WebMvcConfigurer {

  @Override
  public void addCorsMappings(CorsRegistry registry) {
    registry.addMapping("/api/**")
      // lokal + Koyeb (und optional Custom Domain später)
      .allowedOriginPatterns(
        "http://localhost:5173",
        "http://127.0.0.1:5173",
        "https://*.koyeb.app",
        "https://*.koyeb.com",
        "https://*"
      )
      .allowedMethods("GET", "POST", "OPTIONS")
      .allowedHeaders("*")
      // falls du später Downloads per Header machst:
      .exposedHeaders("Content-Disposition")
      .maxAge(3600);
  }
}
