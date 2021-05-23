<template>
  <ul id="menuTreeUL" :style="rootStyle">
    <li>
      <a v-if="mobile" class="caret" @touchstart="onClickCaret">
        {{tree.root.titolo}}</a>
      <span v-else class="caret" @click="onClickCaret">{{tree.root.titolo}}</span>
      <ul class="nested">
        <div
          :key="nome"
          v-for="(sezione, nome) in tree.root.sezioni">
          <li v-if="sezione.corsi">
              <span v-if="mobile" :mobile="mobile" class="caret" @touchstart="onClickCaret">
                {{sezione.nome}}</span>
              <span v-else :mobile="mobile" class="caret  " @click="onClickCaret">
                {{sezione.nome}}</span>
              <ul class="nested">
                <div
                  :key="sottosezione + nome"
                  v-for="(sottosezione, nome) in sezione.corsi">
                  <div
                    v-if="(sottosezione.video.length + sottosezione.articoli.length) > 0">
                    <span v-if="mobile" class="caret" @touchstart="onClickCaret">
                      {{sottosezione.nome}}</span>
                    <span v-else class="caret" @click="onClickCaret">{{sottosezione.nome}}</span>
                  <li>
                      <ul class="nested">
                        <div
                          :key="articolo + nome"
                          v-for="(articolo, nome) in sottosezione.articoli">
                          <li>
                            <TreeMenuEntry
                            :tipo="'A'"
                            :pubblico="auth || articolo.pubblico"
                            :testo="articolo.nome"
                            @click="$emit(
                              'select-content',
                              sezione.nome,
                              sottosezione.nome,
                              articolo.nome,
                              articolo.contenuto);"/>
                          </li>
                        </div>
                        <div
                          :key="nome"
                          v-for="(curvideo, nome) in sottosezione.video">
                          <li>
                            <TreeMenuEntry
                            :tipo="'V'"
                            :pubblico="auth || curvideo.pubblico"
                            :testo="curvideo.nome"
                            @click="$emit(
                              'select-content',
                              sezione.nome,
                              sottosezione.nome,
                              curvideo.nome,
                              curvideo.link);"/>
                          </li>
                        </div>
                      </ul>
                  </li>
                  </div>
                  <li v-else>{{sottosezione.nome}}</li>
                </div>
              </ul>
          </li>
          <li v-else>
              {{sezione.nome}}
          </li>
        </div>
      </ul>
    </li>
  </ul>
</template>

<script>
import TreeMenuEntry from '@/components/TreeMenuEntry.vue';

export default {
  name: 'TreeMenu',
  components: {
    TreeMenuEntry,
  },
  data() {
    return {
    };
  },
  props: {
    tree: Object,
    auth: Boolean,
    fontSize: String,
  },
  computed: {
    authorized() {
      return this.$store.getters['auth/isAuthorized'];
    },
    mobile() {
      const result = Object.hasOwnProperty.call(window, 'ontouchstart');
      return result;
    },
    rootStyle() {
      return {
        '--font-size': this.$props.fontSize,
      };
    },
  },
  methods: {
    onClickCaret(event) {
      event.target.parentElement.querySelector('.nested').classList.toggle('active');
      event.target.classList.toggle('caret-down');
    },
  },
};
</script>

<style scoped lang="scss">
 /* Remove default bullets */
ul, #menuTreeUL {
  list-style-type: none;
  text-align: left;
  font-size: var(--font-size);
}

/* Remove margins and padding from the parent ul */
#menuTreeUL {
  margin: 0;
  padding: 0;
}

/* Style the caret/arrow */
.caret {
  cursor: pointer;
  user-select: none; /* Prevent text selection */
  display: block;
  width: 100%;
}

/* Create the caret/arrow with a unicode, and style it */
.caret::before {
  content: "\25B6";
  color: black;
  display: inline-block;
  margin-right: 6px;
}

/* Rotate the caret/arrow icon when clicked on (using JavaScript) */
.caret-down::before {
  transform: rotate(90deg);
}

/* Hide the nested list */
.nested {
  display: none;
}

/* Show the nested list when the user clicks on the caret/arrow (with JavaScript) */
.active {
  display: block;
}
</style>
