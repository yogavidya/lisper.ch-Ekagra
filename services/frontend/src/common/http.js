import axios from 'axios';

export default axios.create({
  baseURL:
    document.baseURI.startsWith('http://localhost') ? 'http://localhost:9999/Ekagra-api/' : 'https://lisper.ch/Ekagra-api/',
});
