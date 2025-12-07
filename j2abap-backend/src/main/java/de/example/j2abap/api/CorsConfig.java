@Configuration
public class CorsConfig implements WebMvcConfigurer {
  @Override
  public void addCorsMappings(CorsRegistry registry) {
    registry.addMapping("/api/**")
      .allowedOrigins("https://chijzay.github.io")
      .allowedMethods("GET", "POST", "OPTIONS")
      .allowedHeaders("*");
  }
}
