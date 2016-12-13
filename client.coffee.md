Markov Contact
==============

Markov Contact is an experiment in dynamic music visualisation. It takes Keiji
Yamagishi's wonderful song First Contact and splits it up into its constituent
segments, rearranges them using a Markov model, and visualises them in several
ways.

Firstly, the overall structure and theme of the song is represented in the
layout and connections between the nodes. Secondly, the star indicating the
current node pulses and moves with the beat. Thirdly, the stars in the
background move based on the mood of the section within the song.

This is based on a previous project, Markov Technology, which you can find
here: https://demos.samgentle.com/markov-technology

You can find a writeup of the process of making it here:
https://samgentle.com/posts/2016-12-11-markov-contact

You can find First Contact, and much more of Keiji Yamagishi's work here:
http://store.bravewave.net/album/retro-active-pt-1

In this system, we have six different but equally important groups:

- The Node, which represents the nodes on the screen and the connections
  between them as elements on an SVG canvas

- The Group, which represents the collections of nodes and the constellation-
  style images behind them

- The Audio, which handles playing the music via the Web Audio API

- The Pulser, which is the beat detection engine that makes the current node
  wobble and bounce

- The Starfield, which displays various animations on a canvas behind
  everything else

- And the Director, which coordinates all of the above, choosing which of the
  possible nodes will come up next, transitioning from one to the other, and
  navigating when you click on them

These are their stories...

Utilities
---------

    $ = document.querySelector.bind(document)
    audioContext = new (window.AudioContext || window.webkitAudioContext)

    BPM = 127 # beats per minute
    MSPB = 1000 * 60 / BPM # milliseconds per beat

Simple event emitter. We always duplicate the `listeners` array in case `on`
or `un` are being called in the middle of an event - this would cause strange
behaviour with the array iterator. We could alternatively duplicate the array
when we iterate over it, but there are probably way more calls to `emit` than
`on` or `un`.

    makeEmitter = (obj={}) ->
      listeners = {}
      obj.on = (ev, f) -> (listeners[ev] = listeners[ev]?.slice() or []).push f
      obj.un = (ev, f) -> listeners[ev] = (listeners[ev] or []).filter (_f) -> _f isnt f
      obj.once = (ev, f) ->
        unf = (data) -> f(data); obj.un ev, unf
        obj.on ev, unf
      obj.emit = (ev, data) -> f data for f in listeners[ev] or []
      obj.unlistenAll = -> listeners = {}
      obj

Scoped timers are a nice way of dealing with the problem of maintaining lots
of different `setTimeout`s for things. Any time we need to be able to clear
timers when they're no longer relevant (ie the animation or Markov node
they're for has been changed) we can used scoped timers.

    scopedTimers = ->
      timeouts = []
      intervals = []
      {
        setTimeout: (f, t) -> timeouts.push setTimeout f, t
        setInterval: (f, t) -> intervals.push setInterval f, t
        clear: ->
          clearTimeout t for t in timeouts
          clearInterval t for t in intervals
          timeouts = []
          intervals = []
      }

Audio
-----

An Audio element handles everything to do with loading the audio from the
network and playing it via the Web Audio API. It emits various useful events
and generally paves over the relative hairiness of knowing when the audio
actually starts and stops.

    makeAudio = (src) ->
      buffer = null
      audioNode = null
      nearTime = 500
      timers = scopedTimers()

      getBuffer = ->
        return Promise.resolve buffer if buffer

        fetch(src)
          .then (response) -> response.arrayBuffer()
          .then (audioData) -> new Promise (accept) -> audioContext.decodeAudioData audioData, accept
          .then (_buffer) -> buffer = _buffer; buffer

      load = -> getBuffer()

      play = (at) -> getBuffer().then (buffer) =>
        return if audioNode
        at ?= audioContext.currentTime
        audioNode = audioContext.createBufferSource()
        audioNode.buffer = buffer
        audioNode.connect pulser.node
        audioNode.connect audioContext.destination
        audioNode.start at#, 0, buffer.duration

        @start = at
        @duration = buffer.duration

        audioNode.onended = =>
          console.log 'ended'
          @emit 'ended'

        timers.setTimeout =>
          @emit 'started'
        , (at - audioContext.currentTime) * 1000

        timeout = Math.max(0, Math.round((at - audioContext.currentTime + buffer.duration) * 1000 - nearTime))
        timers.setTimeout =>
          console.log "near", Date.now() % 100000
          @emit 'near'
        , Math.max(0, (at - audioContext.currentTime + buffer.duration) * 1000 - nearTime)

      playAfter = (otherNode) -> @play otherNode.start + otherNode.duration

      stop = ->
        timers.clear()
        @unlistenAll()
        @start = null
        @duration = null
        audioNode?.disconnect()
        audioNode.stop() if @start
        audioNode = null

      makeEmitter {load, play, playAfter, stop}

Pulser
------


The Pulser is our beat detector. It makes the stars pulse with the beat, but
to do so it needs beat detection. That's where the Detector comes in.

The detector uses a biquad filter to find the specific frequency range we want
and then an analyser to pull that data out. We don't actually rely on any
FFT'd frequency domain data here, we just use the amplitudes. In my testing
that was good enough as long as you're precise with the filter.

The detector doesn't make assumptions about the amplitudes that come in.
Instead, it fills up a ring buffer and measures the present amplitude relative
to the minimum and maximum over some range.

    makeDetector = (opts={}) ->
      filter = audioContext.createBiquadFilter()
      analyser = audioContext.createAnalyser()
      analyser.fftSize = opts.fftSize or 512
      filter.connect analyser

      # buffer = new Float32Array(analyser.fftSize)
      buffer = new Uint8Array(analyser.fftSize)
      min = null
      max = null
      ringbufSize = opts.ringbufSize or 360
      samples = new Float32Array(ringbufSize)
      idx = 0

      detect = ->
        # analyser.getFloatTimeDomainData buffer
        analyser.getByteTimeDomainData buffer
        avg = 0
        # avg += Math.abs(val) for val in buffer
        avg += Math.abs(val-128) for val in buffer
        avg /= buffer.length

        min = null
        max = null
        for sample in samples
          min = sample if !min? || sample < min
          max = sample if !max? || sample > max
        samples[idx] = avg
        idx = (idx + 1) % ringbufSize
        Math.round(Math.min((avg-min)/(max-min), 1)*1000)/1000

      {filter, analyser, buffer, detect, in: filter, out: analyser}


The Pulser uses two Detectors, one for the pulsing movement and another for
wobbling. The pulsing is intended to coincide mainly with the kick of the bass
drum, so we use a lowpass filter at a fairly low frequency.

The wobbling is intended to coincide with the tempo, so we look for the
midrange where the melody is strongest, and averages out the times between
exceptional amplitudes to guess at the tempo. There are probably better ways
to do this.

    makePulser = (opts={}) ->
      node = audioContext.createGain()

      beatDetector = makeDetector()
      beatDetector.filter.frequency.value = 100
      beatDetector.filter.Q.value = 1
      beatDetector.filter.type = 'lowpass'
      node.connect beatDetector.in

      melodyDetector = makeDetector ringbufSize: 120, fftSize: 256
      melodyDetector.filter.frequency.value = 1200
      window.freq = melodyDetector.filter.frequency
      melodyDetector.filter.Q.value = 1.66
      window.Q = melodyDetector.filter.Q
      melodyDetector.filter.type = 'bandpass'
      node.connect melodyDetector.in

      drawing = false
      stopped = false
      attachedEl = null
      rot = 0
      rotdir = 1
      rotdecay = 0.90
      lastt = null
      lastbeat = null
      numdiffs = 8
      diffs = new Array(numdiffs)
      diffi = 0
      mindiff = MSPB / 4 # Min is quarter notes
      maxdiff = MSPB * 4 # Max is 1 bar
      rotvelocity = maxdiff

      lastscale = 1

      draw = -> requestAnimationFrame (t) ->
        if stopped
          drawing = false
          return

        beat = beatDetector.detect()
        melody = melodyDetector.detect()

        if lastt
          diff = t - lastt
          if melody > 0.5
            beatdiff = t - lastbeat
            if lastbeat and maxdiff >= beatdiff >= mindiff
              diffs[diffi] = beatdiff
              diffi = (diffi + 1) % numdiffs

              totaldiff = 0
              diffcount = 0
              for thisdiff in diffs when thisdiff?
                totaldiff += thisdiff
                diffcount += 1

              avgdiff = totaldiff / diffcount
              rotvelocity = avgdiff

            lastbeat = t

          if rotvelocity > 0
            rot += (diff / rotvelocity * 30) % 60 * rotdir

          oldrot = rot
          if rot > 30
            rot = 30 - (rot - 30)
            rotdir = -1
          if rot < -30
            rot = -30 - (rot + 30)
            rotdir = 1

        scale = 1 + beat
        newscale = if !lastscale or scale > lastscale then scale else (scale + lastscale) / 2
        attachedEl.style.transform = "scale(#{newscale}) rotate(#{rot}deg)"
        lastscale = newscale

        lastt = t
        draw()

      attach = (node) ->
        stopped = false
        attachedEl = node.el
        draw() unless drawing
        drawing = true

      detach = ->
        stopped = true
        attachedEl = null

      {node, attach, detach}

    pulser = makePulser()

SVG
---

Before we can do any node drawing, we need to set up our SVG canvas.

    svg = document.querySelector('svg')

Making an SVG element is pretty verbose so we use a simple helper.

    svgEl = (name, attribs={}) ->
      el = document.createElementNS 'http://www.w3.org/2000/svg', name
      el.setAttribute k, v for k, v of attribs
      el

The defs are used later for adding gradients.

    defs = svgEl 'defs'
    svg.appendChild defs

There's no z-index in SVG, so we use groups to keep everything on the right
level.

    linkGroup = svgEl 'g',
      'opacity': 1
    nodeGroup = svgEl 'g'
    imageGroup = svgEl 'g'

    svg.appendChild imageGroup
    svg.appendChild linkGroup
    svg.appendChild nodeGroup

Groups
------

The groups are used to set the colours of the nodes, as well as the images
that sit behind them. We also keep the animations for the starfield attached
to the group, since it's mostly a per-group thing.

    groups = {}

    addGroup = (name, group) ->
      groups[name] = group
      group.name = name

      if group.image
        group.imageEl = svgEl 'image',
          href: "images/#{group.image.name}.png"
          x: group.image.x
          y: group.image.y
          width: group.image.w
          height: group.image.h
          style: "transition: opacity 50ms ease-in-out"
          opacity: (if group.image.hidden then 0 else 1)
        group.imageEl.setAttributeNS 'http://www.w3.org/1999/xlink', 'xlink:href', "images/#{group.image.name}.png" #Ew

        imageGroup.appendChild group.imageEl

      group.activate = ->
        group.imageEl.setAttribute 'opacity', 1 if group.image.hidden
      group.deactivate = ->

Links
-----

Links exist as a first-class thing (rather than just being attached to the
node they come from) because we need to mess around with them a lot outside of
the context of a single node.

For example, the pathfinding when you click on a node lights up the links
along the way. Also, when there are links between different groups we show and
hide them as needed.

Making gradients for a line in SVG is awful and I wouldn't wish it on anyone.

    links = do ->
      allLinks = {}

      makeGradient = (from, to) ->
        id = "grad-#{from.name}--#{to.name}"
        linkGrad = svgEl 'linearGradient',
          id: id
          gradientUnits: 'userSpaceOnUse'
          x1: from.x, y1: from.y
          x2: to.x, y2: to.y
        linkStartGrad = svgEl 'stop', offset: '0%', 'stop-opacity': 1, 'stop-color': from.group?.stroke or '#ffffff'
        linkStopGrad = svgEl 'stop', offset: '100%', 'stop-opacity': 1, 'stop-color': to.group?.stroke or '#ffffff'
        linkGrad.appendChild linkStartGrad
        linkGrad.appendChild linkStopGrad
        defs.appendChild linkGrad
        id

      make = (from, to) ->
        el = svgEl 'g',
          stroke: '#fff',
          'stroke-width': 4
        el.style.transition = 'all 0.5s ease-in-out'
        linkGroup.appendChild el

        dx = to.x - from.x
        dy = to.y - from.y
        m = dy / dx
        d = Math.sqrt(dy * dy + dx * dx)
        x1 = from.x + dx * (10/d)
        y1 = from.y + dy * (10/d)
        x2 = to.x - dx * (10/d)
        y2 = to.y - dy * (10/d)

        line = svgEl 'line', {x1, y1, x2, y2}
        el.appendChild line

        if from.group isnt to.group
          crossGroup = true
          el.setAttribute 'stroke-dasharray', '25,20'
          el.setAttribute 'opacity', 0
          gradient = makeGradient from, to

        prepare = ->
          el.setAttribute 'opacity', 1 if crossGroup

        activate = ->
          if crossGroup
            el.setAttribute 'opacity', 1
            el.setAttribute 'stroke', "url(##{gradient})"
          else
            el.setAttribute 'stroke', from.group.stroke or '#ffffff'

          linkGroup.appendChild el

        deactivate = ->
          el.setAttribute 'stroke', 'white'
          el.setAttribute 'opacity', 0 if crossGroup

        {prepare, activate, deactivate}

      add = (from, to) -> allLinks["#{from.name}:#{to.name}"] = make from, to
      get = (from, to) -> allLinks["#{from.name}:#{to.name}"]
      all = (f) -> f(link) for _, link of allLinks
      reset = -> link.deactivate() for _, link of allLinks

      {add, get, all, reset}

Node
----

Finally, we reach our plucky hero: the Node! This is responsible for the
little circles on the SVG canvas, but also stores information related to that
node, like the Audio connected to it and which group it's in.

    lastClicked = null

    nodeProto =
      render: ->
        return if @el
        @el = el = svgEl 'g',
          opacity: 1
          style: "transform-origin: #{@x}px #{@y}px; transform:scale(0.5); cursor: pointer"

        @el.addEventListener 'click', =>
          if lastClicked is this
            director.jump(this)
          else
            director.navigate(this)
          lastClicked = this

        x = @x
        y = @y
        r = 40
        r2 = 20

This is a totally sweet way of generating star shapes. We basically draw two
different-sized n-gons rotated by half an edge from each other and join their
vertices up.

        shapeCoords = (cx, cy, r, steps, offset=0) ->
          for i in [0...steps]
            x: cx + Math.sin(2*Math.PI/steps*i + offset) * r
            y: cy + Math.cos(2*Math.PI/steps*i + offset) * r

        stellated = (cx, cy, r1, r2, steps) ->
          coords1 = shapeCoords cx, cy, r1, steps
          coords2 = shapeCoords cx, cy, r2, steps, Math.PI/steps
          ret = []
          ret.push coords1[i], coords2[i] for i in [0...coords1.length]
          ret

        svgpoints = (points) -> ("#{p.x},#{p.y}" for p in points).join(' ')

        @circle = circle = svgEl 'circle',
          cx: x, cy: y, r: r2
          fill: @group.fill
          stroke: 'black'

        @spokes = spokes = svgEl 'polygon',
          points: svgpoints stellated x, y, r2, r, 9
          fill: @group.fill
          stroke: 'black'
          opacity: 0

        el.appendChild spokes
        el.appendChild circle
        nodeGroup.appendChild el

      activate: ->
        nodeGroup.appendChild @el
        lastClicked = null if lastClicked is this

        @el.setAttribute 'opacity', 1.0
        @el.style.transform = 'scale(1.0)'
        @circle.setAttribute 'fill', @group.stroke
        @spokes.setAttribute 'opacity', 1.0 unless @group.nospokes

      deactivate: ->
        @circle.setAttribute 'fill', @group.fill
        @el.setAttribute 'opacity', 1.0
        @el.style.transform = 'scale(0.5)'
        @spokes.setAttribute 'opacity', 0

      link: (nodes, weight=1) ->
        nodes = [nodes] unless Array.isArray(nodes)
        for node in nodes
          links.add this, node
          next = {node, weight}
          @nexts.push next
        this

    makeNode = (src, data) ->
      o = Object.create nodeProto
      o.src = src
      o.nexts = []
      o.name = src.split('/').pop().split('.')[0]
      o.audio = makeAudio src
      o[k] = data[k] for k in 'x y label rate rates mode'.split(' ')
      o.group = groups[data.group]
      o.render()
      o

Director
--------

The Director doesn't visualise anything, it just sits in the background
pulling all the strings. It's like a mafia boss, but with state transitions
instead of murder.

We're synchronising three separate clocks here: the Web Audio API keeps its
own sample-accurate time for the purposes of sequencing. The SVG and Canvas
use requestAnimationFrame, which is synced to the framerate of the display.
Finally, all the rest of our code is built on setTimeout, which isn't really
synced to anything.

Basically, hold on to your butts.

    director = do ->
      nodeList = []
      currentNode = null
      scheduledNode = null

      nodeTimers = scopedTimers()

We track the current Node and a list of future Nodes. If you don't click
anything, that Node will be chosen randomly here. We do that as early as
possible (just after the previous Node has been activated) to give us time to
load the audio. Then we don't show the user which one we've picked for a
second because ignorance is more fun.

      prepareNext = ->
        if !nodeList.length
          next = getNext()
          return finish() if !next
          nodeList.push next

          for next in currentNode.nexts
            links.get(currentNode, next.node).prepare()

          nodeTimers.setTimeout ->
            links.get(currentNode, nodeList[0]).activate()
          , 1000

        nodeList[0].audio.load()

      getNext = ->
        total = currentNode.nexts.reduce ((total, next) -> total + next.weight), 0
        rand = Math.random()*total
        cur = 0
        for next in currentNode.nexts
          cur += next.weight
          return next.node if rand <= cur

deactivateCurrent/activateNext handle the transitions between nodes, except
for scheduling the Audio.

      deactivateCurrent = ->
        nodeTimers.clear()
        currentNode.deactivate()
        currentNode.group.deactivate()
        currentNode.audio.stop()
        for next in currentNode.nexts
          links.get(currentNode, next.node).deactivate()
        pulser.detach()
        currentNode = null

      activateNext = ->
        deactivateCurrent() if currentNode

        currentNode = nodeList.shift()
        scheduledNode = null

        currentNode.activate()
        currentNode.group.activate()
        pulser.attach currentNode
        updateStarfield()
        prepareNext()

The Audio can't be triggered by a timer, because that would be too easy.
Instead, we have to schedule it in advance. If we've already scheduled it and
we schedule something else, we have to cancel it or else we get lots of audio
at once.

      scheduleNext = ->
        # console.log "scheduleNext from", currentNode?.name, "to", nodeList[0]?.name, "scheduled", scheduledNode?.name
        return prepareNext() unless nextNode = nodeList[0]
        unScheduleNode() if scheduledNode

        if currentNode
          nextNode.audio.playAfter currentNode.audio
        else
          nextNode.audio.play()

        nextNode.audio.once 'near', scheduleNext
        nextNode.audio.once 'started', activateNext

        scheduledNode = nextNode

      unScheduleNode = ->
        scheduledNode.audio.stop()
        scheduledNode = null

When we transition to a new Starfield animation, we sometimes want it to
change more than once during a single Node. This is highly annoying, but looks
cool as hell so we have a little timer thingy to do it here.

      updateStarfield = ->
        starfield.setMode currentNode.mode or currentNode.group.mode or 'static'
        if currentNode.rates
          starfield.setRate currentNode.rates[0]
          for rate, i in currentNode.rates when i isnt 0 then do (rate, i) =>
            nodeTimers.setTimeout ->
              starfield.setRate rate
            , currentNode.audio.duration/currentNode.rates.length * i * 1000
        else
          starfield.setRate currentNode.rate or currentNode.group.rate or 1

No project is complete without a search algo. When Google engineers ask me if
I know programming, I can point them to this function so they can see I have
vital real-world programming skills.

      pathTo = (node) ->
        visited = {}
        visiting = [{node: currentNode}]
        while thisNode = visiting.shift()
          if thisNode.node is node
            path = []
            while thisNode.from
              path.push thisNode.node
              thisNode = thisNode.from
            return path.reverse()

          for next in thisNode.node.nexts when !visited[next.node.name]
            visiting.push { node: next.node, from: thisNode }
            visited[next.node.name] = true

We pathfind if you click on another node.

      navigate = (node) ->
        return jump node if !currentNode
        return unless newNodeList = pathTo node
        nodeList = newNodeList
        scheduleNext()
        prevNode = currentNode

        links.reset()
        for node in nodeList
          links.get(prevNode, node).activate()
          prevNode = node

We jump straight to the node if you're already pathfinding to it and you click
it again. This looks like double-clicking, but it's more general and therefore
better.

      jump = (node) ->
        links.reset()
        starfield.resync()

        deactivateCurrent() if currentNode

        nodeList = [node]
        scheduleNext()

      finish = ->
        currentNode.audio.on 'ended', -> deactivateCurrent() if currentNode

      {navigate, prepareNext, getNext, navigate, jump}

    window.director = director


Starfield
---------

Finally, the Starfield. This is a 2D Canvas with pixels (really, tiny
rectangles) on it. Everything we draw is in terms of rectangles, because
curves are too hard.

The animation runs on a bunch of modes, each roughly corresponding to the
Group we're in. We have a global rate parameter that changes how fast
everything goes, and individual nodes tend to set that when there are tempo
changes within a group.

While the modes change the attributes of each pixel, the pixels themselves are
reused between modes and share a single draw function, meaning you can follow
one pixel through several animations if you want.

    createStarfield = ->
      canvas = $('#starfield')

We can set the scale higher if we want our pixels to be blurrier.

      scale = 2
      canvas.width = canvas.offsetWidth / scale
      canvas.height = canvas.offsetHeight / scale

      window.addEventListener 'resize', ->
        oldWidth = canvas.width
        oldHeight = canvas.height
        canvas.width = canvas.offsetWidth / scale
        canvas.height = canvas.offsetHeight / scale
        ctx.width = canvas.width
        ctx.height = canvas.height
        for p in pixels
          p.x = Math.round(p.x * canvas.width / oldWidth)
          p.y = Math.round(p.y * canvas.height / oldHeight)

      ctx = canvas.getContext '2d'
      ctx.width = canvas.width
      ctx.height = canvas.height
      ctx.fillStyle = 'white'

      pixels = for [1..500]
        x: Math.floor(Math.random()*ctx.width)
        y: Math.floor(Math.random()*ctx.height)
        w: 1
        h: 1
        r: 255
        g: 255
        b: 255
        a: Math.random()

Phase is used in every animation, it's basically what provides the random
variation between pixels. We use it for alpha, we use it for parallax, we use
it for size, colour. Basically there's only one kind of variation, and this is
it.

      p.phase = p.a for p in pixels

      mode = 'static'
      rate = 1

      step = (p, beats) -> currentMode.step(p, beats)

Here are our modes. Most of this code is pretty hairy, but fun to mess around
with. I wanted to make each step function a pure function determined entirely
by the pixel's initial state and where we are in the music. That was too hard,
but I did it for a few of them.

      modes =
        static:
          noRedraw: true
          step: ->

        twinkle:
          step: (p, beats) ->
            level = ((beats + p.phase) * rate) % 2
            if level > 1
              p.a = 2-level
            else
              p.a = level

          teardown: (p) ->
            p.a = p.phase

          transitions:
            fly: ->
              ctx.fillStyle = 'rgba(255, 255, 255, 1)'
              ctx.fillRect 0, 0, ctx.width, ctx.height

        fly:
          setup: (p) ->
            p.h = 5 * scale * rate * p.phase
            p.a = p.phase / 2
          step: (p, beats) ->
            p.y = (p.y + 5 * scale * rate * p.phase) % ctx.height

          fadeRate: 0.1
          teardown: (p) ->
            p.a = p.phase
            p.h = 1

        yaw:
          setup: (p) ->
            p.h = 4 * scale * rate * p.phase
          step: (p, beats) ->
            yaw = (beats / 8 * scale * rate) % 2
            yaw = 1 - yaw if yaw > 1

            p.x = (p.x + Math.max(Math.min(yaw * 4, 0.25),-0.25) * scale * p.phase) % ctx.width
            p.x += ctx.width if p.x < 0
            p.y = (p.y + 4 * rate * scale * p.phase) % ctx.height

          fadeRate: 0.1
          teardown: (p) ->
            p.h = 1
            p.w = 1

        surf:
          setup: (p) ->
            p.w = 2 * p.phase
            p.h = 2 * p.phase
          step: (p, beats) ->
            val = (beats / 2 * rate) % 2
            yaw = Math.sin(val * Math.PI)

            p.x = (p.x + yaw * scale * p.phase) % ctx.width
            p.x += ctx.width if p.x < 0
            p.y = (p.y + scale * p.phase * rate) % ctx.height

          fadeRate: 1
          teardown: (p) ->
            p.w = 1
            p.h = 1

        fall:
          step: (p, beats) ->
            p.h = rate * scale * p.phase
            p.y = (p.y - rate * scale * p.phase)
            p.y += ctx.height if p.y < 0

          fadeRate: 0.25

          teardown: (p) -> p.h = 1

          transitions:
            fly: ->
              for p in pixels
                ctx.fillRect p.x, 0, p.w, ctx.height

        circles:
          setup: (p) ->
            p.w = scale / 2 * p.phase
            p.h = scale / 2 * p.phase
            p.dir = if Math.random() > 0.5 then -1 else 1
          step: (p, beats) ->
            val = (beats / 2 * rate * p.phase + p.phase * 2) % 2
            r = Math.min(rate, 2) * scale / 4
            p.x += Math.sin(val * Math.PI) * r * p.dir
            p.y += Math.cos(val * Math.PI) * r * p.dir
            p.dir *= -1 if Math.random() > 1 - rate * 0.02

          fadeRate: 0.05
          teardown: (p) ->
            p.w = 1
            p.h = 1

        contact:
          setup: ->
            p.r = Math.floor(Math.random()*255)
            p.g = Math.floor(Math.random()*255)
            p.b = Math.floor(Math.random()*255)
            p.ox = p.x
            p.oy = p.y
          step: (p, beats) ->
            level = ((beats / 4 + p.phase) * rate/2) % 2
            if level > 1
              p.a = 2-level
            else
              p.a = level
            p.w = 2*p.a * rate
            p.h = 2*p.a * rate
            p.x = p.ox - p.w/2
            p.y = p.oy - p.h/2
            p.oy = p.oy - rate * Math.min(1, p.phase*2) / 4
            p.oy += ctx.height if p.oy < 0

          fadeRate: 1
          teardown: (p) ->
            p.a = p.phase
            p.r = p.g = p.b = 255
            p.w = 1
            p.h = 1

Normally I try to avoid adding extra properties to the pixels but this was too difficult.

        swirls:
          setup: (p) ->
            p.dir = 1
            p.rad = 1
            p.ox = p.x
            p.oy = p.y
            p.oc = {r: Math.floor(Math.random()*255), g: Math.floor(Math.random()*255), b: Math.floor(Math.random()*255)}
          step: (p, beats) ->
            val = (beats / 4 + p.phase * 2) % 1
            cx = ctx.width / 2
            cy = ctx.height / 2
            p.w = 8 * p.a * (1-val)
            p.h = 8 * p.a * (1-val)

            r = Math.round((Math.min(1, (4 / rate)) + p.phase) / 2 * 255)
            g = Math.round((Math.min(1, (rate / 4)) + (1-p.phase)) / 2 * 255)
            b = Math.round(p.phase * 127)

            p.r = Math.round(r * val * 4 + p.oc.r * (1-val) / 4)
            p.g = Math.round(g * val * 4 + p.oc.g * (1-val) / 4)
            p.b = Math.round(b * val * 4 + p.oc.b * (1-val) / 4)

            p.x = (p.ox * (1-val) + cx * val)
            p.y = (p.oy * (1-val) + cy * val)

          fadeRate: 0.1
          teardown: (p) ->
            p.x = p.ox
            p.y = p.oy
            p.h = p.w = 1
            p.r = p.g = p.b = 255

      currentMode = modes.static

Different animations want different levels of crispness, but we need to
transition smoothly between them. For that reason, we maintain a fadeRate here
that indicates how opaque our blanking between frames is.

Our draw function also keeps track of how many beats have passed based on the
animation frame timing, so we can use that in our animations. We need to
resync this any time we interrupt the music.

      startt = null
      needsResync = true
      currentFadeRate = 1
      targetFadeRate = 1
      draw = -> requestAnimationFrame (t) ->
        if needsResync
          startt = t
          needsResync = false
        diff = t - startt

        if currentFadeRate != targetFadeRate
          if targetFadeRate < currentFadeRate
            currentFadeRate = targetFadeRate
          else
            currentFadeRate = (currentFadeRate * 19 + targetFadeRate) / 20

        ctx.fillStyle = "rgba(0, 0, 0, #{currentFadeRate})"
        ctx.fillRect 0, 0, ctx.width, ctx.height

        last = {}
        for p in pixels
          if p.r != last.r or p.g != last.g or p.b != last.b or p.a != last.a
            ctx.fillStyle = "rgba(#{p.r},#{p.g},#{p.b},#{p.a})"
          ctx.fillRect p.x, p.y, p.w, p.h
          step p, diff / MSPB if diff > 0
          last = p

        draw() unless currentMode.noRedraw and diff > 60

      draw()

setMode handles the tricky transition stuff. Any time we change modes we have
to teardown the old mode, setup the new one, and call any custom transitions
we have set. We also resync on transition changes just in case.

      setMode = (mode) ->
        newMode = modes[mode]
        draw() if currentMode.noRedraw
        currentMode.transitions?[mode]?()
        if currentMode != newMode
          needsResync = true
          for p in pixels
            currentMode.teardown p if currentMode.teardown
            newMode.setup p if newMode.setup
            p.x = Math.round(p.x)
            p.y = Math.round(p.y)

        targetFadeRate = newMode.fadeRate ? 1

        currentMode = newMode

Technically we could handle changing animation speed without resyncing.
Technically we could do a lot of things. Instead we just resync.

      setRate = (_rate) ->
        needsResync = true if rate != _rate
        rate = _rate

      resync = -> needsResync = true

      window.setMode = setMode
      window.setRate = setRate

      {setMode, setRate, resync}

    starfield = createStarfield()

Setup
-----

Thanks for coming on this crazy journey. If you made it this far, here's the
part where we actually load all the data and make it go.

We have to check for ogg support because Firefox seems unable to figure out
mp3 lengths accurately. Also because oggs are better or something.

    do ->
      canOgg = document.createElement('audio').canPlayType('audio/ogg')
      ext = if canOgg then 'ogg' else 'mp3'

      nodes = {}

      fetch('data.json')
      .then (result) -> result.json()
      .then (data) ->
        for groupName, group of data.groups
          addGroup groupName, group

        nodes[k] = makeNode "media/#{k}.#{ext}", v for k, v of data.nodes

        for k, v of data.nodes
          for weight, _links of v.links
            nodes[k].link (nodes[l] for l in _links), Number(weight)

        nodes.countdown_6.audio.load()
      .then ->
        $('#click').style.opacity = 1
        $('#splash').addEventListener 'click', ->
          director.jump(nodes.countdown_6)
          $('#splash').remove()