import { Component } from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'

@Component({
  templateUrl: './message.component.html',
  styleUrls: [ './message.component.css' ]
})
export class MessageComponent {
  messageId: string

  constructor(private _route: ActivatedRoute, private _router: Router) {
    this.messageId = this._route.snapshot.paramMap.get('id')
  }

  onBack(): void {
    this._router.navigate([ '/messages' ])
  }
}
