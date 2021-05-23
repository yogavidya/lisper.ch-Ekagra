<template>
  <div class="home d-flex" :auth="auth">
    <b-button v-if="mobile"
      id="bottone-sidebar"
      class="bottone-sidebar"
      variant="transparent"
      v-b-toggle.sidebar-element>
        Contenuti
      </b-button>
    <b-button v-else
      id="bottone-sidebar"
      class="bottone-sidebar"
      variant="transparent"
      @mouseover="showSidebar"
      v-b-toggle.sidebar-element>
        Contenuti
      </b-button>
      <b-sidebar id="sidebar-element"
      no-header-close
      body-class="sidebar-corpo"
      shadow>
      <template
        v-slot:title="{ hide }">
        <div
          class="b-sidebar-header">
          <b-btn
            variant="outline-info"
            @click="hide">
            Chiudi
          </b-btn>
        </div>
      </template>
      <template v-slot:default>
        <div
          :style="stiliSidebar">
        <TreeMenu
          :tree="treeMenuData"
          :auth="auth"
          :fontSize='touchCell'
          @select-content="onSelectContent"/>
        </div>
      </template>
      </b-sidebar>
    <Content
    :section="section"
    :contenuto="contenuto"
    @content-update="onUpdateContent"/>
  </div>
</template>

<script>
// @ is an alias to /src
import Content from '@/components/Content.vue';
import TreeMenu from '@/components/TreeMenu.vue';

export default {
  name: 'Home',
  props: {
    auth: Boolean,
    fullScreenMode: Boolean,
  },
  components: {
    Content, TreeMenu,
  },
  data() {
    return {
      section: 'Benvenuti',
      contenuto: {
        nomeCorso: '',
        nomeContenuto: '',
        contenuto: '',
      },
    };
  },
  computed: {
    treeMenuData() {
      return this.$store.getters.treeMenuData;
    },
    verticalStart() { return this.$store.getters['document/top']; },
    viewHeight() { return this.$store.getters['document/height']; },
    viewWidth() { return this.$store.getters['document/width']; },
    stiliSidebar() {
      return {
        top: `${this.verticalStart}px`,
        width: '212px',
      };
    },
    mobile() {
      return this.$store.getters['document/mobile'];
    },
    touchCell() { return this.$store.getters['document/touchCell']; },
  },
  methods: {
    onUpdateContent() {
      this.$emit('content-update');
    },
    showSidebar() {
      this.$children[0].localShow = true;
    },
    hideSidebar() {
      this.$children[0].localShow = false;
    },
    onSelectContent(nomeSezione, nomeCorso, nomeContenuto, contenuto) {
      this.hideSidebar();
      this.contenuto = {
        nomeCorso,
        nomeContenuto,
        contenuto,
      };
      this.section = nomeSezione;
    },
  },
};
</script>

<style lang="scss">

@mixin home-common() {
  background-image: url("/Ekagra/photo/sfondo-lago.gif");
  background-size: cover;
  background-blend-mode: lighten;
}

@mixin sidebar-corpo-common () {
  background-image: linear-gradient(#596e8b, lightblue);
}

@media screen and (max-width: 360px) {
  .home {
    @include home-common();
    width: 100vw;
    height: 90vh;
    background-color: rgb(89, 110, 139);
    overflow: auto;
    font-size: 60%;
  }
 #sidebar-element {
    top: calc(10vh + 1px) !important;
    height: calc(90vh - 1px) !important;
    width: 100vw !important;
    overflow: auto;
  }
  .sidebar-corpo {
    @include sidebar-corpo-common();
    background-color: rgb(89, 110, 139);
    opacity: 1;
    overflow: auto;
  }
}
@media screen and (max-width: 720px) and (min-width: 361px) {
  .home {
    @include home-common();
    width: 100vw;
    height: 90vh;
    background-color: rgb(89, 110, 139);
    overflow: auto;
  }
 #sidebar-element {
    top: calc(10vh + 1px) !important;
    height: calc(90vh - 1px) !important;
    width: 100vw !important;
    overflow: auto;
  }
  .sidebar-corpo {
    @include sidebar-corpo-common();
    background-color: rgb(89, 110, 139);
    opacity: 1;
    overflow: auto;
  }
}

@media screen and (min-width: 721px) {
  .home {
    @include home-common();
    width: 100vw;
    height: 80vh;
    // background-color: #9eadbe;
    background-color: rgb(89, 110, 139);
    overflow: auto;
    //background-size: contain;

  }
  #sidebar-element {
    top: calc(20vh + 1px) !important;
    height: calc(80vh - 1px) !important;
    width: 33vw !important;
    overflow: auto;
  }
  .sidebar-corpo {
    @include sidebar-corpo-common();
    background-color: rgb(89, 110, 139);
    opacity: 1;
    overflow: auto;
  }
}

.bottone-sidebar {
  position: sticky;
  top: 0;
  margin-top: 2px;
  writing-mode: sideways-lr;
  text-orientation: sideways;
  box-shadow: 0 .5rem 1rem rgba(0,0,0,.15);
  opacity: 1;
  z-index: 1000;
}
.bottone-sidebar:hover {
  text-shadow: 2px 2px 3px darkblue;
  }

.b-sidebar-header {
  background-color: var(--ekagra-background) !important;
  flex-direction: row-reverse !important;
}
</style>
