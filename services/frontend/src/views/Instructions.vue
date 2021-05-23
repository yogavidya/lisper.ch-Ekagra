<template>
  <div class="istruzioni container-flex">
    <div class="riga-istruzioni row" :key="elemento" v-for="elemento in testo">
      <img v-if="elemento.startsWith('IMMAGINE: ')"
        :style="imageStyleVars"
        class="immagine-istruzioni"
        :src="elemento.match(/(?:^IMMAGINE:\W)(.+$)/)[1]"/>
      <span v-else v-html="elemento"/>
    </div>
  </div>
</template>

<script>
// @ is an alias to /src
export default {
  name: 'Instructions',
  props: [
    'auth',
  ],
  components: {
  },
  data() {
    return {
      testo: [
        `Nella pagina <strong>Home</strong>, a sinistra dello schermo c'è un menu a scomparsa con l'elenco dei contenuti:
        articoli e video, divisi in sezioni e corsi.`,
        'Le voci in blu sono accessibili, quelle in grigio no.',
        `Se usate il sito da PC, per aprire il menu dei contenuti basta spostare il puntatore sul bottone <strong>Contenuti</strong>,
        mentre su cellulare o tablet dovete toccarlo.`,
        'IMMAGINE: /Ekagra/photo/istruzioni/menu-contenuti-chiuso.jpg',
        `Il menu dei contenuti rimane aperto finché non selezionate un articolo o un video,
        oppure cliccate sul bottone <strong>Chiudi</strong> in alto a destra nel menu.`,
        'IMMAGINE: /Ekagra/photo/istruzioni/menu-contenuti-aperto.jpg',
        'Una volta abbonati, avrete accesso a tutti i contenuti.',
        'Per abbonarvi, dal menu nella barra in alto selezionate <strong>Accesso/Registrazione</strong>.',
        'IMMAGINE: /Ekagra/photo/istruzioni/dialogo-registrazione.jpg',
        `Una volta inseriti i pochi dati richiesti, verrete rediretti su <strong>PayPal</strong>, dove potete autorizzare
        il pagamento mensile della quota per Ekagra Yoga.`,
        `Completata l'autorizzazione, potete accedere al sito (<strong>Accesso/Accedi...</strong>)
        con le vostre credenziali.`,
        `Se disdicete il pagamento mensile su PayPal, automaticamente verrà annullato
        anche l'accesso completo a
        Ekagra Yoga e i vostri dati verranno cancellati dall'archivio.`,
        `<strong>Perché conviene abbonarsi?</strong> Anche solo i video delle lezioni - decine di ore, che nel tempo
        aumenteranno - richiedono tempi molto lunghi per essere appresi e assimilati con una pratica attenta.`,
        `Io sono disponibile con piacere per ogni allieva o allievo: se mi contattate su <strong>ekagra.yoga@lisper.ch</strong>,
        risponderò alle vostre domande sulla pratica. Non solo, ma se mi accorgo che una richiesta tocca un tema
        importante che ha bisogno di approfondimento, aggiungerò video o articoli
        secondo le necessità. `,
        `Non è come vedersi di persona, ma farò del mio meglio per essere presente nella vostra pratica individuale.
        Buon lavoro con Ekagra Yoga!`,
      ],
    };
  },
  computed: {
    top() { return this.$store.getters['document/top']; },
    height() { return this.$store.getters['document/height']; },
    width() { return this.$store.getters['document/width']; },
    margin() { return this.$store.getters['document/margin']; },
    remPixels: () => window.getComputedStyle(document.body, null)['font-size'],
    marginPx() {
      return parseInt(this.margin, 10)
      * parseInt(
        this.remPixels,
        10,
      );
    },
    padding() { return this.$store.getters['document/padding']; },
    paddingPx() {
      return parseInt(this.padding, 10)
      * parseInt(
        this.remPixels,
        10,
      );
    },
    mobile() { return this.$store.getters['document/mobile']; },
    orientation() { return this.$store.getters['document/orientation']; },
    imageStyleVars() {
      if (this.orientation === null) {
        this.$store.commit('document/update');
      }
      const hDivisor = this.orientation === 'landscape' ? 3 : 2;
      const vDivisor = this.orientation === 'portrait' ? 2 : 1.5;
      const actualWidth = this.width - this.marginPx - this.paddingPx;
      const maxWidth = actualWidth / hDivisor;
      return {
        '--max-width': `${maxWidth}px`,
        '--max-height': `${(this.height - this.marginPx - this.paddingPx)
          / vDivisor}px`,
        '--margin': `${this.margin}`,
        '--margin-left': `${(actualWidth - maxWidth) / 2.5}px`,
      };
    },
  },
  methods: {
  },
};
</script>

<style lang="scss">

@media screen and (max-width: 360px) {
  .istruzioni {
    width: 100vw;
    height: 90vh;
    background-color: rgb(89, 110, 139);
    overflow: auto;
    font-size: 60%;
    text-align: justify;
    padding: 3rem;
  }
  .riga-istruzioni {
    margin-bottom: 0.5em;
  }
}
@media screen and (max-width: 720px) and (min-width: 361px) {
  .istruzioni {
    width: 100vw;
    height: 90vh;
    background-color: rgb(89, 110, 139);
    overflow: auto;
    text-align: justify;
    padding: 3rem;
  }
  .riga-istruzioni {
    margin-bottom: 0.5em;
  }
}
@media screen and (min-width: 721px) {
  .istruzioni {
    width: 100vw;
    height: 80vh;
    // background-color: #9eadbe;
    background-color: rgb(89, 110, 139);
    overflow: auto;
    text-align: justify;
    padding: 3rem;
  }
  .riga-istruzioni {
    margin-bottom: 0.5em;
  }
}
.immagine-istruzioni {
  border-style: none;
  max-width: var(--max-width);
  max-height: var(--max-height);
  height: 100%;
  margin-top: var(--margin);
  margin-bottom: var(--margin);
  margin-left: var(--margin-left);
  box-shadow: 1px 1px 5px 0px rgba(0,0,0,0.75);
}
</style>
