{ div, h1, p, a, form, thead, th, td, tr, table, tbody, input, li, ul } = React.DOM

descriptionIntro = "This box displays the item that was used to ask about the currently hovered characteristic."

descriptionMap =
  skill: "How much skill is required?"
  unpredict: "How unpredictable is the action?"
  novelty: "To what extent do you find new and fresh experiences during the activity?"
  creative: "To what extent are there opportunities to express creativity during the activity?"
  complex: "How complex is the action and the environment in which it takes place?"
  goal1: "How clear and straightforward are the short-term rules and goals?"
  feedback1: "How quickly do you know whether what you wanted to accomplish in the short term was successful?"
  goal2: "How clear and straightforward are the long-term rules and goals?"
  feedback2: "How quickly do you know whether what you wanted to accomplish in the long term was successful?"
  chatter: "Does your extraneous mental chatter temporarily shut down during the activity?"
  waiting: "How much time do you feel like is spent waiting for the next step in the activity?"
  body: "How much of your body is involved in the activity? Is it full-body or only part of your body?"
  control: "How much control do you feel like you can exert over your performance?"
  present: "During the activity, to what extent do you feel like you are in the present moment?"
  spont: "How spontaneous is your action?"
  stakes: "During the activity, how important does the activity feel?"
  evaluated: "To what extent do you care about how others judge your performance?"
  reward: "After engaging in the physical activity, to what extent do you feel great?"
  injury1: "What is the risk of minor physical injury (more than just muscle soreness)?"
  injury2: "What is the risk of serious physical injury?"

weightLine = (props) ->
    tr { key: props.key },
      td
        onMouseEnter: () ->
          $("#charHelp").stop(true)
          $("#charHelp").fadeTo(0, 1.0)
          props.updateHelp(props.facet)
        props.facet
      td {},
        input
          type: "radio"
          name: props.facet
          value: -1
          onChange: () -> props.onClick(props.index, -1)
      td {},
        input
          type: "radio"
          name: props.facet
          value: -0.5
          onChange: () -> props.onClick(props.index, -0.5)
      td {},
        input
          type: "radio"
          name: props.facet
          value: 0
          defaultChecked: true
          onChange: () -> props.onClick(props.index, 0)
      td {},
        input
          type: "radio"
          name: props.facet
          value: 0.5
          onChange: () -> props.onClick(props.index, 0.5)
      td {},
        input
          type: "radio"
          name: props.facet
          value: 1.0
          onChange: () -> props.onClick(props.index, 1.0)

weightPanel = (props) ->
  lis = RCPA_FACETS.map((text, index) ->
    weightLine
      facet: text
      key: text
      index: index
      onClick: props.onClick
      updateHelp: props.updateHelp)
  div
    className: "fixed seven wide column"
    div
      className: "ui message"
      onMouseLeave: () =>
        props.updateHelp(null)
        $("#charHelp").delay(10000).fadeOut('slow')
      div
        className: "header"
        "Which characteristics do you value?"
      table
        className: "ui selectable celled table"
        thead {},
          tr {},
            th {}, "Characteristic"
            th {}, "-1.0"
            th {}, "-0.5"
            th {}, "0.0"
            th {}, "0.5"
            th {}, "1.0"
        tbody {},
          lis

weightPanelHelp = (props) ->
  div
    id: "charHelp"
    className: "overlay fixed padded"
    style:
      position: "fixed"
      width: "350px"
      right: "10px"
      top: "10px"
    div
      className: "ui message"
      div
        className: "header"
        "Description"
      props.facetDescription

dotprod = (v1, v2) ->
  throw new Error("dotprod: #{v1.length} != #{v2.length}") if v1.length isnt v2.length
  maxIndex = v1.length - 1
  out = 0
  out += v1[x] * v2[x] for x in [0..maxIndex] when (v1[x] isnt "NA" and v2[x] isnt "NA")
  out

resultPanel = (props) ->
  maxPA = RCPA_PA.length - 1
  score = new Array(RCPA_PA.length)
  score[x] = dotprod(props.weight, RCPA_DATA[x]) for x in [0..maxPA]
  order = [0..maxPA]
  order.sort (a,b) ->
    cmp = score[a] - score[b]
    if cmp is 0
      cmp = -1 if RCPA_PA[a] < RCPA_PA[b] 
      cmp = 1  if RCPA_PA[a] > RCPA_PA[b] 
    cmp
  lis = order.map((index) ->
    div
      className: "item"
      key: index
      a
        href: "https://en.wikipedia.org/wiki/#{encodeURI(RCPA_PA[index])}"
        target: "_blank"
        RCPA_PA[index])
  div
    className: "three wide column"
    div
      className: "ui list"
      lis

class Jumbotron extends React.Component
  constructor: (props) ->
    super(props)
    @state =
      facetDescription: descriptionIntro
      weight: new Array(RCPA_FACETS.length).fill(0)

  handleChange: (index, value) =>
    foo = @state.weight.slice()
    foo[index] = value
    this.setState
      weight: foo

  updateHelp: (current) =>
    if current is null
      this.setState
        facetDescription: descriptionIntro
      return
    this.setState
      facetDescription: descriptionMap[current]

  render: ->
    fn = this.handleChange
    fn2 = this.updateHelp
    div {},
      weightPanelHelp
        facetDescription: @state.facetDescription
      div
        className: "ui grid padded"
        weightPanel
          onClick: (index, value) ->
            fn(index, value)
          updateHelp: (current) => fn2(current)
        resultPanel
          weight: @state.weight

ReactDOM.render(React.createElement(Jumbotron), document.getElementById('content'))
