<template>
  <div>
      <h1 class="admin-header">
        "With great powers come great responsibilities"
        </h1>
      <b-button-group v-if="this.datiOk"
      size="lg"
      vertical>
        <b-button
        v-b-modal.video-modal>
          Inserisci nuovo video
        </b-button>
        <ModalNewVideo
          :optionsCorsi="optionsCorsi"
          @submit-video="onSubmitModalNewVideo"
          @annulla-video="onAnnullaModalNewVideo"/>
        <b-button
        v-b-modal.articolo-modal>
          Inserisci nuovo articolo
        </b-button>
        <ModalNewArticle
          :optionsCorsi="optionsCorsi"
          @submit-articolo="onSubmitModalNewArticolo"
          @annulla-articolo="onAnnullaModalNewArticolo"/>
      </b-button-group>
  </div>
</template>

<script>
import http from '@/common/http';
import ModalNewVideo from '@/components/ModalNewVideo.vue';
import ModalNewArticle from '@/components/ModalNewArticle.vue';

export default {
  name: 'AdminHome',
  components: {
    ModalNewVideo, ModalNewArticle,
  },
  beforeMount() {
    http.get('lista-corsi', {
      headers: {
        authorization: `Bearer ${this.$store.getters['auth/token']}`,
      },
    })
      .then((result) => {
        this.listaCorsi = result.data;
        result.data.forEach((entry) => {
          this.optionsCorsi.push({
            value: entry.id,
            text: entry.nome,
          });
        });
        this.queryRunning = false;
      })
      .catch((err) => {
        // eslint-disable-next-line
        alert(err);
        this.queryRunning = false;
      });
  },
  data() {
    return {
      queryRunning: false,
      listaCorsi: null,
      optionsCorsi: [],
    };
  },
  computed: {
    datiOk() {
      return this.listaCorsi;
    },
  },
  props: {
  },
  methods: {
    onSubmitModalNewVideo(event) {
      // event.preventDefault();
      http.post('store-new-video', {
        nome: event.nome,
        link: event.link,
        corso: event.idCorso,
        sequenza: event.sequenza,
        pubblico: event.pubblico,
      },
      {
        headers: {
          authorization: `Bearer ${this.$store.getters['auth/token']}`,
        },
      })
        .then((result) => {
          // eslint-disable-next-line
          window.alert(`result: ${result.toString()}`)
          this.$emit('content-update');
        })
        .catch((err) => {
          // eslint-disable-next-line
          alert(`Error, returned: ${err}`);
        });
    },
    closeAndResetVideoModal() {
      this.videoForm = {
        nome: '',
        link: '',
        idCorso: '',
        sequenza: 0,
        pubblico: false,
      };
      this.$bvModal.hide('video-modal');
    },
    onAnnullaModalNewVideo(event) {
      event.preventDefault();
      this.closeAndResetVideoModal();
    },
    onSubmitModalNewArticolo(event) {
      http.post('store-new-article', {
        nome: event.nome,
        contenuto: event.contenuto,
        corso: event.idCorso,
        sequenza: event.sequenza,
        pubblico: event.pubblico,
      },
      {
        headers: {
          authorization: `Bearer ${this.$store.getters['auth/token']}`,
        },
      })
        .then((result) => {
          // eslint-disable-next-line
          window.alert(`result: ${result.toString()}`)
          this.$emit('content-update');
        })
        .catch((err) => {
          // eslint-disable-next-line
          alert(`Error, returned: ${err}`);
        });
    },
    closeAndResetArticoloModal() {
      this.articoloForm = {
        nome: '',
        contenuto: '',
        idCorso: '',
        sequenza: 0,
        pubblico: false,
      };
      this.$bvModal.hide('articolo-modal');
    },
    onAnnullaModalNewArticolo(event) {
      event.preventDefault();
      this.closeAndResetArticoloModal();
    },
  },
};
</script>
<style lang="scss" scoped>
.ad-header {
  margin: 1rem;
  font-style: italic;
}
</style>
