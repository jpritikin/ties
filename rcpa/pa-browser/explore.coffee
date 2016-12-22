{ div, h1, h2, h3, p, a, i, select, option, label, form, button,
 thead, th, td, tr, table, tbody, input, li, ul } = React.DOM

descriptionIntro = "This box displays the item that was used to ask
  about the currently hovered characteristic."

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

Skills=["Novice", "Amateur", "Expert"]

RCPA_DATA=
  Novice: RCPA_DATA1
  Amateur: RCPA_DATA2
  Expert: RCPA_DATA3

weightLine = (props) ->
  items = [-1,-.5,0,.5,1].map((val) ->
    option
      key: val
      value: val
      val)
  tr
    key: props.facet
    td {},
      if props.abbreviateFacet then props.facet else descriptionMap[props.facet]
    td {},
      select
        name: props.facet
        value: props.weight
        onChange: (event) ->
          props.onClick(props.index, event.target.value)
        items

weightPanel = (props) ->
  lis = RCPA_FACETS.map((text, index) ->
    weightLine
      facet: text
      abbreviateFacet: props.abbreviateFacet
      index: index
      onClick: props.onClick
      weight: props.weight[index])
  div
    className: "fixed seven wide column"
    div
      className: "ui message"
      div
        className: "header"
        "Which characteristics do you value?"
      table
        className: "ui selectable celled table"
        thead {},
          tr {},
            th {}, "Characteristic"
            th {}, "Weight"
        tbody {},
          lis
      div
        className: "inline field"
        div
          className: "ui toggle checkbox"
          input
            className: "hidden"
            type: "checkbox"
            checked: props.abbreviateFacet
          label
            onClick: ->
              props.setAbbreviateFacet(!props.abbreviateFacet)
            "Abbreviate characteristics"

dotprod = (v1, v2) ->
  throw new Error("dotprod: #{v1.length} != #{v2.length}") if v1.length isnt v2.length
  maxIndex = v1.length - 1
  out = 0
  out += v1[x] * v2[x] for x in [0..maxIndex] when (v1[x] isnt "NA" and v2[x] isnt "NA")
  out

prettyName = (name) ->
  part = name.split(';')
  return name if part.length is 1
  return "#{part[0]} (#{part[1]})" if part.length is 2
  throw new Error("How to prettyName #{name}?")

resultPanel = (props) ->
  maxPA = RCPA_PA.length - 1
  score = new Array(RCPA_PA.length)
  score[x] = dotprod(props.weight, RCPA_DATA[props.skill][x]) for x in [0..maxPA]
  order = [0..maxPA]
  order.sort (a,b) ->
    cmp = score[a] - score[b]
    if cmp is 0
      cmp = -1 if RCPA_PA[a] < RCPA_PA[b]
      cmp = 1  if RCPA_PA[a] > RCPA_PA[b]
    cmp
  lis = order.map((index) ->
    rawName = RCPA_PA[index]
    part = rawName.split(';')
    div
      className: "item"
      key: index
      a
        href: "https://en.wikipedia.org/wiki/#{encodeURI(part[0])}"
        target: "_blank"
        prettyName(RCPA_PA[index]))
  div
    className: "three wide column"
    div
      className: "ui buttons"
      Skills.map((text) ->
        isActive = ""
        isActive = "active" if text is props.skill
        div
          className: "ui button #{isActive}"
          key: text
          onClick: () ->
            props.setSkill(text)
          text)
    div
      className: "ui list"
      lis

class Jumbotron extends React.Component
  constructor: (props) ->
    super(props)
    @state =
      abbreviateFacet: false
      weight: new Array(RCPA_FACETS.length).fill(0)
      skill: Skills[1]

  handleChange: (index, value) =>
    foo = @state.weight.slice()
    foo[index] = value
    this.setState
      weight: foo

  setAbbreviateFacet: (current) =>
    this.setState
      abbreviateFacet: current

  setCurrentSkill: (value) =>
    this.setState
      skill: value

  render: ->
    fn = this.handleChange
    fn2 = this.setAbbreviateFacet
    fn3 = this.setCurrentSkill
    div {},
      h1
        className: "ui header center aligned"
        "Relative characteristics of physical activities:"
      h2
        className: "ui header center aligned"
        "Preliminary results"
      div
        className: "ui text container"
        h3
          className: "ui header"
          "Introduction"
        "Flow is a state in which an individual is completely immersed in
        an activity without reflective self-consciousness but with a deep sense of control.
        We selected 20 characteristics of activities that are thought to relate to flow.
        One goal of this study is figure out which physical activities are more or less conducive to flow.
        However, in the short term, we invite you to select characteristics that you care about.
        Physical activities will be ranked according to your priorities.
        Which physical activities that you haven’t tried yet might be worth considering?"
        h3
          className: "ui header"
          "Explorer"
        "These results are obtained by fitting responses from participants with a statistical model.
        This model considers each characteristic independently of other characteristics.
        For example, creativity is ranked independently of novelty.
        Currently, sample size is very limited and rankings may not match intuition very well.
        Model estimates should improve with additional data."
      div
        className: "ui grid padded"
        weightPanel
          onClick: (index, value) ->
            fn(index, value)
          abbreviateFacet: @state.abbreviateFacet
          setAbbreviateFacet: fn2
          weight: @state.weight
        resultPanel
          weight: @state.weight
          skill: @state.skill
          setSkill: (current) -> fn3(current)

ReactDOM.render(React.createElement(Jumbotron), document.getElementById('content'))
