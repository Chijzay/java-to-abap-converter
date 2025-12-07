# Java → ABAP online (Subset-Translator)

## Lokales Testen

### Backend
```bash
cd backend
mvn -q spring-boot:run
# API: http://localhost:8080/api/translate
```

### Frontend
Öffne `frontend/index.html` (z.B. mit VS Code Live Server) und setze in `frontend/app.js` `API_BASE`.

## Deployment
- Frontend: GitHub Pages via `.github/workflows/pages-frontend.yml`
- Backend: Fly.io via `.github/workflows/deploy-backend-fly.yml` (Secret `FLY_API_TOKEN`)
