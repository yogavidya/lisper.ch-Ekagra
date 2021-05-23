<template>
  <div
    :style="styleVars"
    class="benvenuti">
    <div v-if="this.$store.getters['auth/nickname'] === 'admin'">
      <AdminHome
      @content-update="$emit('content-update');"/>
    </div>
    <div v-else class="ey-contenuti-sezione">
      <h1>Benvenuti al sito di Ekagra - corso di Yoga online</h1>
      <h3>(Versione preliminare)</h3>
      <h2 v-if="this.$store.getters['auth/isAuthorized']">
        Sono disponibili i contenuti per abbonati</h2>
      <div v-else>
        <h2>
          Tre videolezioni di Yoga gratuite.</h2>
          <h2>
            <router-link to="access?registration=true">Registrati</router-link> per l'intera libreria!
          </h2>
      </div>
    </div>
    <div>
      <h3>
        Seleziona un video o un articolo dal menu "Contenuti" alla sinistra.
      </h3>
    </div>
  </div>
</template>

<script>
import AdminHome from '@/components/AdminHome.vue';

export default {
  name: 'Benvenuti',
  components: {
    AdminHome,
  },
  methods: {
    getDocAttr(name) {
      return this.$store.getters[`document/${name}`];
    },
  },
  computed: {
    styleVars() {
      return {
        '--background-color': 'var(--ekagra-background-color)',
        '--margin': this.getDocAttr('margin'),
        '--padding': this.getDocAttr('padding'),
      };
    },
  },
};
</script>

<style lang="scss" scoped>
.benvenuti {
  width: 100vw;
  background-color: var(--background-color);
  margin: var(--margin);
  padding: var(--padding);
  text-shadow: 1px 1px 1px lightblue;
}
</style>
