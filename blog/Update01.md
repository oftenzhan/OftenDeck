## The OftenDeck Update

My adventures with adjusting and modifying my writerdeck (using a Raspberry Pi Zero 2W) has been a frustratingly fun process. Here is my progress and thoughts so far.

### Philosophy behind A WriterDeck

The idea of a writerdeck has always fascinated me. A device made for writing. I briefly commented about it in a reddit forum post asking, *"What is the philosophy behind a writerdeck?"* 

In it, I wrote:

> The writing process has many stages—pre-writing, drafting, writing, editing, revising, and publishing. Among these, the writerdeck really stands out for drafting.
> 
> The idea behind the writerdeck goes way back to old typewriters and early word processors like the Tandy or Alphasmart. These devices were built for one thing: getting words on the page. They stripped away distractions, making it easier for writers to focus on drafting without worrying about formatting or extra features. That’s why people often call them "distraction-free"—they’re designed to help you focus on writing without all the unnecessary noise.
> 
> A good way to think about it is like a hammer. Sure, you could drive a nail with a rock, but a hammer is specifically designed for the job. With the top-heavy weight and ergonomic handle, it makes the task effortless. When you look at a hammer, you immediately know what it’s for. When you hold a hammer, you know it's intended purpose. A writerdeck works the same way. The moment you pick it up, you know it’s made for one thing: writing. Unlike a phone, which is full of distractions, or a laptop with a massive screen and countless apps, a writerdeck keeps things simple, streamlined, and focused.
> 
> If Maslow's Hammer says, "If you are holding a hammer, everything looks like a nail," then Maslow's Writerdeck would be, "If you are holding a writerdeck, everything looks like a story to be written."
> 
> As the writerdeck community grows, I think it will start to take on a bigger role in the overall writing process. Right now, the focus is mostly on the hardware—building writerdecks—because no device has truly stepped in as the rightful, modern successor to the Alphasmart. But as the community matures and reliable writerdecks become commercially available, the conversation will shift toward software. Right now, we’re focusing on the "deck" side of the writerdeck, but over time, we’ll start talking more about the "writer"—the writing process itself and how the writerdeck can support it.

### My Device: MicroJournal Rev.2.ReVamp

I had been on the lookout for a writerdeck to fiddle with for the past several years. I've owned a Pomera, an AlphaSmart Neo, a GPD Pocket 2, several typewriters, and a slew of other devices, but none of them really scratch that desire for a modern portable physical word processor. 

### WriterDeck
Then, quite recently, a new subreddit was formed called WriterDeck, which makes devices specifically for writing. It was a play off of words from CyberDeck, which originally came from a book called Neuromancer by William Gibson published in 1984. The word surged in popularity from the video game Cyberpunk 2077 became synonymous with the retro-futurist 80s ideal of a laptop. How would a tech-enthusiast from the 80s and early 90's envision a perfect portable device. 

Back then, Internet was dreadfully slow, so information was primarily sent through text. This was the pre-`Web 1.0` era with BBS, Telnet, DOS TUI, and Usenet. At this time, it was not centric to images, but it was Text-Based Internet Era. The perspective of the internet was text-based. Having the internet be completely image-based was not in the social consciousness of the general public, and if it was, it was only within the perspective of speculative science fiction.

Then, the non-programmer creating writing people—specifically the AlphaSmart community—joined in. They made a play on the word CyberDeck, calling their device a WriterDeck. A device made for writing.

### Purchasing my MicroJournal
It hadcome to a point where I had been seriously considering making my own (because nothing in the market met my preferences of what a WriterDeck should be) until I found the Microjournal Rev.2.ReVamp.

It has (mostly) everything I wanted in a micro deck:

- Clamshell design to protect screen and keys
- a screen with a wide aspect ration that feels the entire top of the clamshaled design. As in, id doesn't leave a lot of dead margin space to the left and right of the hinged lid.
- Mechanical keyboard with hot-swappable keycaps
	- Ortholinear keycaps (this makes it easier to use relegendary keycaps)
- Cheap, easily accessible, upgradable electronics (like a raspberry pi)
- Linux-based so that I can easily configure it to what I needed to do. It needs to be easily editable and configurable, instead of closed off like many Android phones.
- non-proprietary battery - It uses an 18650 battery. (Sadly, it doesn't have good battery life. But it works well with an external battery.)

Essentially, the MicroJournal is a device made from commonly found electronics—a Raspberry Pi Zero 2W, a mechanical keyboard, an appropriately-sized screen, and an 18650 battery—that fit neatly in a compact, portable form factor.

So, when I saw the Microjournal Rev.2.ReVamp, I purchased it on the spot. (Actually, it was a few months later. The inventor, the venerable *Un Kyu Lee*, sells them once a week on Thursdays from Italy, and they were always sold out by the time I tried to clicked purchase. I actually took time off of work to sit near my computer with my credit card info 
saved, spamming refresh on his web storefront until it said the MicroDeck was in stock and available for purchase.)

### Self-Restrictions

I put several restrictions on myself from the start in using and configuring my writerdeck:

- Only open-source
- Keyboard only, no mouse.
- Terminal Only

Originally, I stubbornly restricted myself to TTY-only. But after trying to work within this constraint, I've had unresolvable issues with only using TTY terminal including:

- Certain keybindings are inaccessible,
	- `Control-;`, `Control-,`, and `Control-.`
	- The `tab` key in TTY is unreliable for shortcuts.
- I cannot have adjustable font-size or italics without doing weird unicode hacks.
- Live-preview for Latex (PDF & math equations), markdown (rich-formatting) is both clunky and extremely difficult.

Last week, I realizing that my TTY-only mindset was getting in the way of making a practical Writerdeck.

Now, I've shifted to Terminal only, which encapsulates GUI terminals as well. Not a full blown windows-management system, but just a simple x-system where a terminal is left at full screen at all times.

Some of these issues can be mitigated with `fbterm` or `kmscom`, but installing a minimal X System for only `xterm` or `kitty` keeps the spirit of my vision of a writerdeck.

### Emacs NoX

I like Emacs. I like the idea behind it. It is the closest thing to a modern full terminal-based operating system available. But...

Emacs-nox has given me so much heartache.

The reason why I chose Emacs was because I thought it was made with TTY in mind. Yet the more I researched and played around, I soon discovered that it was a: `You should use the GUI version. If you can't because you have to ssh into it, then emacs-nox is the next best thing.` At least, that was my impression looking at videos and for support; Emacs GUI took priority over the terminal version; terminal emacs are sloppy seconds. I downloaded `emacs-nox` expecting a Scooby Doo, but I found to my chagrin that much of the Emacs community has delegated Emacs-Nox as a Scrappy Doo.

Much of my distress come from the small screensize of my MicroJournal. Since my screen is so small, the screen real estate is more expensive than California's. I removed the menu bar on top—relyong on keybindings—and minimized the modeline and minibuffer like. Many emacs plugins were not designed for such small yet short, wide screens. Tetris doesn't work. Org-Capture is barely usable. Many plugins rely on horizontal splitting, that even when I adjust `split-window-sensibly`, horizontal splits often occur.

~~If there was a way to entirely disable any possibility of horizontal screen splitting, the 50% of my problems would be fixed.~~ Actually, now that I am typing this out, I have an idea. Would this work?

```
(defun my-force-vertical-split (&optional window size side)
  "Force every split to be vertical."
  (window--delete window)
  (split-window (or window (selected-window)) size 'right))

(advice-add 'split-window :override #'my-force-vertical-split)
```

***EDIT**: Hilarious. It works. This is the power of writing out your thoughts. This must be the sketchiest Emacs configuration I have ever made. I essentially said, make all horizontal screen splits into vertical screen splits. I know that this will break something down the line, but it works elegantly for now.*

### Slowly but Surely

Right now, I am focusing on the drafting phase of writing, which is the strong point of the AlphaSmart and the Traveller FreeWrite.

Afterwards, I plan to focus on the pre drafting phase.

Then I will work on editing, then revising, and lastly, publishing.

Here is a chart of things that I am working on:

#### Writerdeck Checklist

###### Pre-Writing
- [ ] **Research**
  - [ ] The Zettelkasten Method
  - [ ] PARA
  - [ ] Commonplace Notebook
  - [ ] Historical Accuracy (if applicable)
  - [ ] Sensory Details (sights, sounds, textures)
  - [ ] Genre Conventions
- [ ] Templating
- [ ] Backup and Sharing
- [ ] World & Character Building
- [ ] Outlining (structural overview, beats, acts)
- [ ] Idea Incubation (letting ideas percolate before drafting)
- [ ] Moodboarding (visual, musical, or thematic inspiration)

##### First Draft
- [ ] Distraction-Free Writing
- [ ] Template Writing
- [ ] Sprint Writing (timed or word-count-driven bursts)
- [ ] Placeholder System (for unresolved details, e.g., `[NAME]`)
- [ ] Voice Consistency Check (narrator & characters)

## Editing
- [ ] Spell-check
- [ ] Grammar-check
- [ ] Read Aloud Pass (for flow and clarity)
- [ ] Sentence Structure Variety (avoiding monotony)
- [ ] Passive vs. Active Voice Check
- [ ] Pacing Check (is it dragging or rushing?)

## Revising
- [ ] Prose Checklist
- [ ] Identifying and Auto-Highlighting Literary Devices
- [ ] Thesaurus and Word Lists
- [ ] Theme Consistency Check
- [ ] Character Arc Evaluation
- [ ] Dialogue Realism & Distinction
- [ ] Sensory Engagement Check (sight, sound, smell, taste, touch)
- [ ] Word Frequency Review (to avoid repetition)

## Publishing
- [ ] LaTeX
- [ ] Printing
- [ ] Live Preview
- [ ] Publish to Blog Post
- [ ] Format Testing (mobile, e-reader, PDF)
- [ ] Cover Design & Typography Check
- [ ] Metadata & Keywords Optimization
- [ ] Query Letter (if submitting to publishers)

## Writing Practices
- [ ] Daily Journal
- [ ] Copy Work
- [ ] Reverse Outlining (summarizing existing drafts to check structure)
- [ ] Daily Writing Log (tracking word count & progress)
- [ ] Writing Challenges (e.g., NaNoWriMo, 100-word prompts)
- [ ] Deep Reading of Stylistic Models