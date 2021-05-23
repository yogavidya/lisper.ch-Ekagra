<template>
  <div v-if="contenuto.contenuto.length > 0"
    :style="contentStyleVars"
    class="sezione-pratica">
    <div v-if="!isVideo">
      <h1 v-if="this.contenuto.nomeCorso">
        {{this.contenuto.nomeCorso}}
      </h1>
      <h2 v-if="this.contenuto.nomeContenuto">
        * {{this.contenuto.nomeContenuto}}
      </h2>
      <div class="riquadro-contenuto-articolo"
      v-html="this.contenuto.contenuto"></div>
    </div>
    <div v-else-if="isVideo">
      <b-btn id="btn-full-screen"
        v-if="isVideo && !isFullScreen"
        @click="onClickFullScreen">
        Schermo intero
      </b-btn>
      <div
      :style="contentStyleVars"
      class="riquadro-contenuto-pratica"
      v-if="this.contenuto.contenuto"
      v-html="videoFrame">
      </div></div>
    </div>
  <Benvenuti v-else/>
</template>

<script>
import Benvenuti from '@/components/Benvenuti.vue';

export default {
  name: 'Content',
  components: {
    Benvenuti,
  },
  data() {
    return {
    };
  },
  props: {
    contenuto: Object,
  },
  computed: {
    isFullScreen: {
      cache: false,
      get: () => document.fullscreenElement,
    },
    isVideo() {
      return this.$props.contenuto.contenuto.startsWith('<iframe');
    },
    availRoom() {
      let w;
      let h;
      if (this.isFullScreen) {
        w = window.screen.availWidth;
        h = window.screen.availHeight;
      } else {
        w = this.$store.getters['document/width'] - this.sidebarButtonWidth;
        h = this.$store.getters['document/height'] - this.fullscreenButtonHeight;
      }
      return {
        w, h,
      };
    },
    videoFrame() {
      if (!this.isVideo) return null;
      return this.contenuto.contenuto
        .replace(/width="[0-9]+"/,
          `width="${this.availRoom.w}" height="${this.availRoom.h}"`);
    },
    sidebarButtonWidth:
      () => Math.ceil(document.getElementById('bottone-sidebar').getBoundingClientRect().width),
    fullscreenButtonHeight:
      () => {
        const fsBtn = document.getElementById('btn-full-screen');
        if (fsBtn) return Math.ceil(fsBtn.getBoundingClientRect().height);
        const elBtn = document.createElement('button');
        elBtn.classList.add('btn');
        elBtn.innerHTML = 'H';
        elBtn.style.position = 'fixed';
        document.body.appendChild(elBtn);
        const h = elBtn.offsetHeight + 8;
        document.body.removeChild(elBtn);
        return h;
      },
    videoIFrame() {
      if (!this.isVideo) {
        return '';
      }
      const desiredWidthStr = `width="${this.$store.getters['document/width'] - this.sidebarButtonWidth}"`;
      const desiredHeightStr = `height="${this.$store.getters['document/height'] - this.fullscreenButtonHeight}"`;
      const tmpStr1 = this.$props.contenuto.contenuto
        .replace(/width="[0-9]+"/, desiredWidthStr);
      const tmpStr2 = tmpStr1
        .replace(/height="[0-9]+"/, desiredHeightStr);
      return tmpStr2;
    },
    contentStyleVars() {
      const margin = this.isVideo ? 0 : this.$store.getters['document/margin'];
      const padding = this.isVideo ? 0 : this.$store.getters['document/padding'];
      const bgColor = this.isVideo ? 'var(--ekagra-background)'
        : 'var(--ekagra-text-background)';
      return {
        '--margin': margin,
        '--padding': padding,
        '--background-color': bgColor,
      };
    },
  },
  methods: {
    fsXitHandler() {
      if (document.fullscreenElement === null) {
        this.isFullScreen = false;
      } else {
        this.isFullScreen = true;
      }
    },
    onClickFullScreen(ev) {
      if (!document.body.getAttribute('fullscreenExitHandler')) {
        document.body.setAttribute('fullscreenExitHandler', true);
        document.body.addEventListener('fullscreenchange', this.fsXitHandler);
      }
      ev.currentTarget.parentElement.requestFullscreen()
        .then(() => {
        })
        .catch((err) => {
          console.debug(err);
        });
    },
  },
};
</script>

<style lang="scss" scoped>
.sezione-pratica {
  background-color: var(--ekagra-background);
  margin: var(--margin);
  padding: var(--padding);
  height: max-content;
  color: black !important;
}
.riquadro-contenuto-pratica {
  background-color: var(--background-color);
  padding: var(--padding);
  margin: var(--margin);
}
.riquadro-contenuto-articolo {
  background-color: var(--background-color);
  padding: var(--padding);
  margin: var(--margin);
  text-align: justify;
}
</style>
