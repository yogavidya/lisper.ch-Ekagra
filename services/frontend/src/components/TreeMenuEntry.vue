<template>
  <div>
    <b-button v-if="pubblico"
    @click="$emit('click')"
    :style="stileMenuEntry(tipo, pubblico)"
    variant="outline"
    size="sm">
      <span style="display: inline-flex">
      <b-badge variant='outline'  style="float: left;">
        <img v-if="tipo==='A'"
        src="/Ekagra/icons/noun_read.svg"
        style="height: 1.3rem; "/>
        <img v-else
        src="/Ekagra/icons/noun_video.svg"
        style="height: 1.3rem; "/>
      </b-badge>
      <span style="float: right;">{{testo}}</span>
      </span>
    </b-button>
    <span v-else
    :style="stileMenuEntry(tipo, pubblico)">
      {{testo}}
    </span>
  </div>
</template>

<script>
// Video by Aybige from the Noun Project
// read by beth bolton from the Noun Project
export default {
  name: 'TreeMenuEntry',
  props: {
    tipo: String, // "V" | "A"
    pubblico: Boolean,
    testo: String,
  },
  data() {
    return {
    };
  },
  methods: {
    stileMenuEntry(tipo, pubblico) {
      return {
        'white-space': 'nowrap',
        'background-color': pubblico ? '#5f89c4' : '#596e8b',
        opacity: pubblico ? 1 : 0.5,
        cursor: pubblico ? 'pointer' : 'not-allowed',
      };
    },
  },
};
</script>
<style lang="scss" scoped>
</style>
