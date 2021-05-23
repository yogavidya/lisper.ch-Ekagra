# build stage
FROM node:lts-alpine as build-stage
WORKDIR /nginx
COPY nginx-configuration/default.conf ./
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY . .
RUN npm run build


# production stage
FROM nginx:alpine as production-stage
COPY --from=build-stage /app/dist /usr/share/nginx/html
RUN rm /etc/nginx/conf.d/default.conf
COPY --from=build-stage /nginx/default.conf /etc/nginx/conf.d/
EXPOSE 8090:8090
CMD ["nginx", "-g", "daemon off;"]