<template>
  <div class="page404">
    <h1>Ha! Sei riuscit* a trovare una pagina che non esiste sul sito!</h1>
  </div>
</template>

<script>
// @ is an alias to /src
export default {
  name: 'Page404',
  data() {
    return {
    };
  },
  computed: {
    top() { return this.$store.getters['document/top']; },
    height() { return this.$store.getters['document/height']; },
    width() { return this.$store.getters['document/width']; },
    mobile() { return this.$store.getters['document/mobile']; },
    orientation() { return this.$store.getters['document/orientation']; },
  },
  methods: {
  },
};
</script>

<style lang="scss">

@media screen and (max-width: 360px) {
  .page404 {
    width: 100vw;
    height: 90vh;
    background-color: rgb(89, 110, 139);
    overflow: auto;
    font-size: 60%;
    text-align: justify;
    padding: 3rem;
  }
}
@media screen and (max-width: 720px) and (min-width: 361px) {
  .page404 {
    width: 100vw;
    height: 90vh;
    background-color: rgb(89, 110, 139);
    overflow: auto;
    text-align: justify;
    padding: 3rem;
  }
}
@media screen and (min-width: 721px) {
  .page404 {
    width: 100vw;
    height: 80vh;
    // background-color: #9eadbe;
    background-color: rgb(89, 110, 139);
    overflow: auto;
    text-align: justify;
    padding: 3rem;
  }
}
</style>
