<template>
  <b-modal id="video-modal" title="Nuovo video" hideFooter hideHeaderClose>
    <b-form>
      <b-form-group id="video-fg-nome" label="Nome" label-for="video-nome">
        <b-form-input
          id="video-nome"
          type="text"
          v-model="videoForm.nome"
          required
          placeholder="Nome del video"
        />
      </b-form-group>
      <b-form-group id="video-fg-link" label="link" label-for="video-link">
        <b-form-input
          id="video-link"
          type="text"
          v-model="videoForm.link"
          required
          placeholder="Codice di embed da Vimeo"
        />
      </b-form-group>
      <b-form-group id="video-fg-corso" label="corso" label-for="video-corso">
        <b-form-select
            v-model="videoForm.idCorso"
            required
            :options="this.optionsCorsi">
            </b-form-select>
      </b-form-group>
      <b-form-group id="video-fg-sequenza" label="numero d'ordine" label-for="video-sequenza">
        <b-form-input
          id="video-sequenza"
          type="number"
          v-model="videoForm.sequenza"
          required
          placeholder="Numero d'ordine del video nel corso"
        />
      </b-form-group>
      <b-form-group id="video-fg-pubblico" label="pubblico" label-for="video-pubblico">
        <b-form-checkbox
          id="video-pubblico"
          v-model="videoForm.pubblico"
          name="checkbox-1"
          :value="true"
          :unchecked-value="false"
        ></b-form-checkbox>
      </b-form-group>
    </b-form>
    <b-button @click="onSubmit" variant="primary">Invia</b-button>
    <b-button @click="onAnnulla" variant="danger">Annulla</b-button>
  </b-modal>
</template>

<script>
export default {
  name: 'ModalNewVideo',
  data() {
    return {
      videoForm: {
        nome: '',
        link: '',
        idCorso: '',
        sequenza: 0,
        pubblico: false,
      },
    };
  },
  props: {
    optionsCorsi: {
      type: Array,
    },
  },
  methods: {
    onSubmit() {
      this.$emit('submit-video', this.videoForm);
    },
    onAnnulla() {
      this.$emit('annulla-video');
    },
  },
};
</script>
<style lang="scss" scoped>
</style>
