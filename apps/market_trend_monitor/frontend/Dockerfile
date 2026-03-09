FROM node:20-alpine AS build

WORKDIR /app

COPY apps/market_trend_monitor/frontend/package*.json ./
RUN npm ci --legacy-peer-deps

COPY apps/market_trend_monitor/frontend/ ./
COPY agentflow/i18n/frontend/ /agentflow/i18n/frontend/

ARG VITE_API_BASE_URL=
ENV VITE_API_BASE_URL=${VITE_API_BASE_URL}

RUN npm run build

FROM nginx:1.27-alpine

RUN rm /etc/nginx/conf.d/default.conf

COPY apps/market_trend_monitor/frontend/nginx.conf /etc/nginx/conf.d/default.conf

COPY --from=build /app/dist /usr/share/nginx/html

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
