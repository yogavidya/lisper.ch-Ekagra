<template>
<!-- VERSIONE LANDSCAPE -->
<div v-if="this.orientation === 'landscape'" style="display: flex;">

  <div id="slideshow-container" class="preview-box"
  v-bind:style="this.previewBoxStyle">
    <Slideshow
      id="About-slideshow"
      :width="this.layout.width.slideshow.toString()"
      :height="this.layout.height.slideshow.toString()"
      :photo="this.photoLandscape"/>
  </div>

  <div :style="this.aboutBoxStyle" class="about-box">
    <img src="../assets/ritratto.jpg"
      style="width: 25%;margin: 1rem;"><br>
    <p v-for="line in aboutText" :key="line">
      {{line}}
    </p>
  </div>
</div>

<!-- VERSIONE PORTRAIT -->
<div v-else>

  <div id="slideshow-container" class="preview-box"
  v-bind:style="this.previewBoxStyle">
    <Slideshow
      id="About-slideshow"
      :width="this.layout.width.slideshow.toString()"
      :height="(this.layout.height.slideshow - 15).toString()"
      :photo="this.photoPortrait"/>
  </div>

  <div :style="this.aboutBoxStyle" class="about-box">
    <img src="../assets/ritratto.jpg"
      style="width: 25%;margin: 1rem;"><br>
    <div>
      <p v-for="line in aboutText" :key="line">
        {{line}}
      </p>
    </div>
  </div>


</div>
</template>

<script>
import Slideshow from '@/components/Slideshow.vue';

export default {
  name: 'About',
  data() {
    return {
      photoLandscape: [
        {
          url: '/Ekagra/photo/padmasana.jpg',
          description: 'Posizione del Loto',
          note: 'Sardegna, 2002',
        },
        {
          url: '/Ekagra/photo/vrksasana.jpg',
          description: 'Posizione dello Scorpione',
          note: 'Sardegna, 2002',
        },
        {
          url: '/Ekagra/photo/parivritta-sirsasana.jpg',
          description: 'Posizione sulla testa in torsione divaricata',
          note: 'Sardegna, 2002',
        },
      ],
      photoPortrait: [
        {
          url: '/Ekagra/photo/urdhva-dhanurasana.jpg',
          description: 'Posizione del Ponte',
          note: 'Sardegna, 2002',
        },
        {
          url: '/Ekagra/photo/ardha-matsyendrasana.jpg',
          description: 'Posizione di Matsyendra',
          note: 'Sardegna, 2002',
        },
        {
          url: '/Ekagra/photo/virabhadrasana-III.jpg',
          description: 'Posizione dell\'eroe',
          note: 'Sardegna, 2002',
        },
      ],
      aboutText: [
        `Mi chiamo Salvatore Uras, sono nato nel 1963.
    Sono una persona... insolita, diciamo.`,
        `A 18 anni ho iniziato la pratica dello Yoga e delle arti marziali,
    spinto dal bisogno di discipline che mi permettessero di esplorare il mondo interiore
    in maniera rigorosa e affidabile, così come le scienze fisiche esplorano il mondo della
    materia.`,
        'Non ho più smesso.',
        `A 39 anni ho iniziato la libera professione come fisioterapista e terapista Shiatsu,
    e per 10 anni mi sono occupato di terapia manuale e rieducazione neuromotoria nelle
    patologie neurologiche.`,
        `Nel 2012 ho smesso di lavorare a tempo pieno come fisioterapista, per occuparmi
    di sviluppo software.`,
        `In tutto questo tempo non ho mai smesso di praticare, studiare e insegnare lo Yoga
    anche se a volte ho diradato la frequenza per esigenze di vita e lavoro.
    Per quanto la mia vita sia ricca - a volte per inclinazione, a volte per forza di cose -
    di interessi anche molto diversi tra loro, lo Yoga è la sua colonna portante,
    ciò che la tiene insieme, e tra tutte le mie attività è quella che mi ha sempre donato
    gioia e benessere, attingendo ad una miniera apparentemente infinita.
    Se non dovessi lavorare per mantenermi, probabilmente sarebbe la mia attività principale:
    sono costretto, per mantenere me stesso e la mia famiglia, a vendere il mio tempo e il mio
    lavoro, ma non sono un commerciante.`,
        `Il prezzo che chiedo per le mie lezioni è una necessità: ma ho sempre cercato
    di fare del mio insegnamento un dono per chi mi ascolta.`,
      ],
    };
  },
  components: {
    Slideshow,
  },
  computed: {
    top() { return this.$store.getters['document/top']; },
    height() { return this.$store.getters['document/height']; },
    width() { return this.$store.getters['document/width']; },
    orientation() { return this.$store.getters['document/orientation']; },
    mobile() { return this.$store.getters['document/orientation']; },
    layout() {
      return {
        width: {
          slideshow: this.orientation === 'landscape' ? Math.ceil(this.width / 3) : this.width,
          about: this.orientation === 'landscape' ? Math.ceil((2 * this.width) / 3) : this.width,
        },
        height: {
          slideshow: this.orientation === 'landscape' ? this.height : Math.ceil(this.height / 3),
          about: this.orientation === 'landscape' ? this.height : Math.ceil((2 * this.height) / 3),
        },
      };
    },
    previewBoxStyle() {
      return {
        '--width': `${this.layout.width.slideshow}px`,
        '--height': `${this.layout.height.slideshow - 15}px`,
        '--top': `${this.top}px`,
        '--padding': '5px',
        '--box-shadow': '10px 10px 5px 0px rgba(0,0,0,0.75)',
      };
    },
    aboutBoxStyle() {
      return {
        '--background-color': '#f0f0f0',
        '--width': `${this.layout.width.about}px`,
        '--height': `${this.layout.height.about - 15}px`,
        '--top': `${this.top}px`,
        '--padding': '2rem',
        '--display': this.layout.orientation === 'landscape' ? 'inline-block' : 'block',
      };
    },

  },
};
</script>
<style lang="scss" scoped>
.preview-box {
  width: var(--width);
  height: var(--height);
  top: var(--top);
  padding: var(--padding);
  box-shadow: var(--box-shadow);
}
.about-box {
  background-color: var(--background-color);
  width: var(--width);
  height: var(--height);
  top: var(--top);
  padding: var(--padding);
  display: var(--display);
  overflow: auto;
  text-align: justify;
}
</style>
