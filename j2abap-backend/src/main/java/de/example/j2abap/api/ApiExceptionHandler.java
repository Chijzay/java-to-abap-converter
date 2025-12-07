package de.example.j2abap.api;

import com.github.javaparser.ParseProblemException;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestControllerAdvice
public class ApiExceptionHandler {

  @ExceptionHandler(ParseProblemException.class)
  public ResponseEntity<String> handleParse(ParseProblemException e) {
    return ResponseEntity.badRequest()
        .contentType(MediaType.TEXT_PLAIN)
        .body("Parse error (Java). Tipp: Snippet braucht gültige Statements (meist mit ';').\n\n" + e.getMessage());
  }
}
