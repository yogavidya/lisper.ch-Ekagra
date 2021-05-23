<template>
  <b-modal id="articolo-modal" title="Nuovo articolo" hideFooter hideHeaderClose>
    <b-form>
      <b-form-group id="articolo-fg-nome" label="Nome" label-for="articolo-nome">
        <b-form-input id="articolo-nome" type="text" v-model="articoloForm.nome" required
          placeholder="Nome dell'articolo" />
      </b-form-group>
      <b-form-group id="articolo-fg-contenuto" label="contenuto" label-for="articolo-contenuto">
        <Editor id="article-editor" :init="{
          height: 200,
          auto_focus: true,
          image_caption: true,
          image_advtab: true,
          plugins: [
    'advlist autolink lists link image charmap print preview anchor',
    'searchreplace visualblocks code fullscreen',
    'insertdatetime media table paste code help wordcount'
  ],
  toolbar:
    'undo redo | formatselect | bold italic backcolor | \
    alignleft aligncenter alignright alignjustify | \
    bullist numlist outdent indent | removeformat | help'
        }"
  :initial-value="contenuto"/>
      </b-form-group>
      <b-form-group id="articolo-fg-corso" label="corso" label-for="articolo-corso">
        <b-form-select v-model="articoloForm.idCorso" required :options="this.optionsCorsi">
        </b-form-select>
      </b-form-group>
      <b-form-group id="articolo-fg-sequenza" label="numero d'ordine" label-for="articolo-sequenza">
        <b-form-input id="articolo-sequenza" type="number" v-model="articoloForm.sequenza" required
          placeholder="Numero d'ordine dell'articolo nel corso" />
      </b-form-group>
      <b-form-group id="articolo-fg-pubblico" label="pubblico" label-for="articolo-pubblico">
        <b-form-checkbox id="articolo-pubblico" v-model="articoloForm.pubblico" name="checkbox-1" :value="true"
          :unchecked-value="false">
        </b-form-checkbox>
      </b-form-group>
    </b-form>
    <b-button @click="onSubmit" variant="primary">Invia</b-button>
    <b-button @click="onAnnulla" variant="danger">Annulla</b-button>
  </b-modal>
</template>

<script>
import Editor from '@tinymce/tinymce-vue';

export default {
  name: 'ModalNewArticle',
  components: {
    Editor,
  },
  data() {
    return {
      articoloForm: {
        nome: '',
        contenuto: '',
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
    contenuto: {
      type: String,
      default: '',
    },
  },
  methods: {
    onSubmit() {
      this.articoloForm.contenuto = window.tinymce.activeEditor.getContent();
      this.$emit('submit-articolo', this.articoloForm);
    },
    onAnnulla() {
      this.$emit('annulla-articolo');
    },
  },
};
</script>
<style lang="scss" scoped>
</style>
