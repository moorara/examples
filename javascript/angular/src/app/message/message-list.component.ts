import { Component, OnInit } from '@angular/core'

import { IMessage } from '../common/message.interface'
import { MessageService } from '../common/message.service'

@Component({
  templateUrl: './message-list.component.html',
  styleUrls: [ './message-list.component.css' ]
})
export class MessageListComponent implements OnInit {
  messages: IMessage[] = []
  errorMessage: string

  constructor(private _messageService: MessageService) {
  }

  ngOnInit(): void {
    this._messageService.getMessages()
      .subscribe(
        messages => this.messages = messages,
        err => this.errorMessage = <any>err
      )
  }
}
